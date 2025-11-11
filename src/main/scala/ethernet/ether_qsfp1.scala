package omnixtend

import chisel3._
import chisel3.util._
import chisel3.dontTouch

import TLOEEtherQSFP1Constants._
import OmniXtendConstants._

// ========================================================================
// Queue Entry Structures
// ========================================================================
/**
 * TX Queue Entry structure
 * Stores TLOE frame data and flit size for transmission
 */
class TLOETxQueueEntry extends Bundle {
  //val data = UInt(TLOE_PACKET_SIZE.W)  // Original packet size (unused)
  val data = UInt(TLOE_FRAME_SIZE.W)     // TLOE frame data (768 bits)
  val flitSize = UInt(7.W)               // Number of flits in the frame
}

/**
 * RX Queue Entry structure
 * Stores received packet data and flit size
 */
class TLOERxQueueEntry extends Bundle {
  val data = UInt(TLOE_PACKET_SIZE.W)   // Received packet data (880 bits)
  val flitSize = UInt(7.W)               // Number of flits received
}

// ========================================================================
// Constants for TLOE Ethernet QSFP1 Module
// ========================================================================
object TLOEEtherQSFP1Constants {
  // ========================================================================
  // Queue Configuration
  // ========================================================================
  val TX_QUEUE_DEPTH = 2  // Number of packets that can be queued for transmission
  val RX_QUEUE_DEPTH = 2  // Number of packets that can be queued for reception
  
  // ========================================================================
  // Packet Size Configuration
  // ========================================================================
  val MAX_CHUNKS = (TLOE_PACKET_SIZE+16) / 64  // = (880+16) / 64 = 896 / 64 = 14 chunks

  // ========================================================================
  // MAC Address Configuration
  // ========================================================================
  val MAC_ADDR_01 = "h001232FFFFFC".U(48.W)  // MAC address for device 1
  val MAC_ADDR_02 = "h001232FFFFFA".U(48.W)  // MAC address for device 2

  // ========================================================================
  // Ethernet Type Configuration
  // ========================================================================
  val ETHER_TYPE = 0xaaaa.U(16.W)  // Custom ethernet type for TLOE packets
}

/**
 * TLOEEtherQSFP1 Module
 * 
 * This module handles the conversion between TLOE frames and Ethernet packets.
 * It manages:
 * - TX: Converts TLOE frames to Ethernet packets with MAC headers
 * - RX: Receives Ethernet packets, validates them, and extracts TLOE frames
 * 
 * Key optimizations:
 * - Uses single registers instead of Vec to reduce LUT usage
 * - 64-bit aligned packet format (896 bits = 14 * 64)
 */
class TLOEEtherQSFP1 extends Module {
  val io = IO(new Bundle {
    // ========================================================================
    // Ethernet Interface - External connection to Ethernet PHY/MAC
    // ========================================================================
    val txdata = Output(UInt(64.W))   // 64-bit Ethernet TX data
    val txvalid = Output(Bool())      // TX data valid signal
    val txlast = Output(Bool())        // Last word in packet
    val txkeep = Output(UInt(8.W))    // Byte enable mask for last word
    val txready = Input(Bool())       // TX ready signal from Ethernet layer

    // ========================================================================
    // Internal TX Interface - From TLOE transmitter
    // ========================================================================
    val internalTxData = Input(UInt(TLOE_FRAME_SIZE.W))    // TLOE frame to transmit
    val internalTxFlitSize = Input(UInt(7.W))               // Number of flits in frame
    val internalTxValid = Input(Bool())                     // TX data valid
    val internalTxReady = Output(Bool())                    // Ready to accept TX data

    // ========================================================================
    // Ethernet RX Interface - From Ethernet PHY/MAC
    // ========================================================================
    val rxdata = Input(UInt(512.W))   // 512-bit Ethernet RX data
    val rxvalid = Input(Bool())        // RX data valid signal
    val rxlast = Input(Bool())         // Last word in received packet
   
    // ========================================================================
    // RX Interface to Endpoint - To TLOE receiver
    // ========================================================================
    val endpointRxData = Output(UInt(TLOE_FRAME_SIZE.W))   // Extracted TLOE frame
    val endpointRxValid = Output(Bool())                    // Frame valid signal
    val endpointRxFlitSize = Output(UInt(7.W))              // Number of flits in frame
    val endpointRxReady = Input(Bool())                     // Endpoint ready to receive

    // ========================================================================
    // Connection Mode
    // ========================================================================
    val modeConn = Input(UInt(2.W))    // Connection mode: 0=idle, 1=master, 2=slave
  })

  // ========================================================================
  // MAC Address and Ethernet Type Registers
  // ========================================================================
  val srcMac = RegInit(0.U(48.W))      // Source MAC address
  val destMac = RegInit(0.U(48.W))      // Destination MAC address
  val etherType = RegInit(0.U(16.W))    // Ethernet type field

  val modeConn = RegInit(0.U(2.W))
  modeConn := io.modeConn

  // ========================================================================
  // MAC Address Initialization Logic
  // ========================================================================
  // Initialize MAC address and etherType based on connection mode
  val isMacSet = RegInit(false.B)
  when (!isMacSet && modeConn =/= MODE_IDLE) {
    when (modeConn === MODE_MASTER) {
      // Master uses MAC_ADDR_02, sends to MAC_ADDR_01
      srcMac := MAC_ADDR_02
      destMac := MAC_ADDR_01
    }.otherwise {
      // Slave uses MAC_ADDR_01, sends to MAC_ADDR_02
      srcMac := MAC_ADDR_01
      destMac := MAC_ADDR_02
    }
    etherType := ETHER_TYPE

    isMacSet := true.B
  }

  // ========================================================================
  // TX/RX Queues
  // ========================================================================
  // TX/RX Queue modules using Chisel Queue
  val txQueue = Module(new Queue(new TLOETxQueueEntry, TLOEEtherQSFP1Constants.TX_QUEUE_DEPTH, pipe=false, flow=false))
  val rxQueue = Module(new Queue(new TLOERxQueueEntry, TLOEEtherQSFP1Constants.RX_QUEUE_DEPTH, pipe=false, flow=false))

  // ========================================================================
  // TX State Machine and Registers
  // ========================================================================
  // Internal TX interface registers
  val internalTxIdx = RegInit(0.U(4.W))        // Current chunk index (0-13 for 14 chunks)
  val internalTxReady = RegInit(true.B)         // TX ready flag

  // MASSIVE LUT REDUCTION: Use single register instead of Vec
  // Removes Cat() operations that consume huge LUTs
  // Add 16 bits padding to align to 64-bit boundary (880 + 16 = 896 = 14 * 64)
  val etherTloePacket = Reg(UInt((TLOE_PACKET_SIZE + 16).W))  // Complete Ethernet packet with padding
  //val internalTxDataReg = RegInit(0.U(TLOE_FRAME_SIZE.W))  // Unused
  val internalTxFlitSizeReg = RegInit(0.U(7.W))  // Flit size for current packet
  val internalTxReadyReg = RegInit(true.B)      // Internal ready flag (unused)
  
  // Default outputs
  io.txdata := 0.U
  io.txvalid := false.B
  io.txlast := false.B
  io.txkeep := 0.U
  io.internalTxReady := txQueue.io.enq.ready
  
  // Default queue signals
  txQueue.io.deq.ready := false.B
  rxQueue.io.deq.ready := false.B
  
  // Default RX queue enqueue signals
  rxQueue.io.enq.valid := false.B
  rxQueue.io.enq.bits.data := 0.U
  rxQueue.io.enq.bits.flitSize := 0.U
  
  // Default RX queue dequeue signals
  rxQueue.io.deq.ready := false.B

  // Default RX outputs
  io.endpointRxData := 0.U
  io.endpointRxValid := false.B
  io.endpointRxFlitSize := 0.U

  // ========================================================================
  // TX Queue Enqueue Logic
  // ========================================================================
  // Handle incoming packets and add to TX queue
  // Input Packet to TX queue
  txQueue.io.enq.valid := io.internalTxValid
  txQueue.io.enq.bits.data := io.internalTxData
  txQueue.io.enq.bits.flitSize := io.internalTxFlitSize

  // ========================================================================
  // TX State Machine
  // ========================================================================
  val etherTxIdle :: etherTxPrepare :: etherTxSend :: etherTxDone :: Nil = Enum(4)
  val etherTxState = RegInit(etherTxIdle)
  
  val etherTxReady = RegInit(true.B)  // Ready to process next packet

  when (txQueue.io.deq.valid && etherTxReady) {
    etherTxReady := false.B
    etherTxState := etherTxPrepare
  }

  // ========================================================================
  // TX State Machine Logic
  // ========================================================================
  switch (etherTxState) {
    // ========================================================================
    // etherTxIdle: Wait for packet in queue
    // ========================================================================
    is (etherTxIdle) {
    }

    // ========================================================================
    // etherTxPrepare: Prepare packet with Ethernet header
    // ========================================================================
    is (etherTxPrepare) {
      val txEntry = txQueue.io.deq.bits

      internalTxIdx := 0.U
      // Add 16-bit padding to align to 64-bit boundary
      // Packet format: [srcMAC(48)][destMAC(48)][etherType(16)][TLOE_frame(768)][padding(16)]
      etherTloePacket := Cat(srcMac, destMac, etherType, txEntry.data, 0.U(16.W))
      // Ensure minimum flit size of 4 (header + minimum data)
      internalTxFlitSizeReg := Mux(txEntry.flitSize < 4.U, 4.U, txEntry.flitSize)

      txQueue.io.deq.ready := true.B
      etherTxState := etherTxSend
    }

    // ========================================================================
    // etherTxSend: Send packet in 64-bit chunks
    // ========================================================================
    is (etherTxSend) {
      // +4 is for the TloeHeader and mask (4 flits = 256 bits)
      val flitSize = internalTxFlitSizeReg + 4.U

      when (io.txready === true.B) {
        when(internalTxIdx < flitSize) {
          // Extract 64-bit chunk - direct access, no Cat()
          // Packet is now 896 bits (TLOE_PACKET_SIZE + 16) to align to 64-bit boundary
          val bitPosition = (internalTxIdx * 64.U)
          val packetSize = (TLOE_PACKET_SIZE + 16).U
          val chunk64 = (etherTloePacket >> (packetSize - 64.U - bitPosition))(63, 0)

          io.txvalid := true.B
          io.txdata := TloePacGen.toBigEndian(chunk64)  // Convert to big-endian for Ethernet
        
          // Check if this is the last chunk
          when(internalTxIdx === (flitSize - 1.U)) {
            io.txlast := true.B
            io.txkeep := "h3F".U  // Last 6 bytes valid (48 bits)
          }.otherwise {
            io.txlast := false.B
            io.txkeep := "hFF".U  // All 8 bytes valid
          }

          internalTxIdx := internalTxIdx + 1.U
        }.elsewhen (internalTxIdx >= flitSize) {
          // All chunks sent, clean up
          io.txdata := 0.U
          io.txvalid := false.B
          io.txlast := false.B
          io.txkeep := 0.U 

          internalTxIdx := 0.U
          etherTxState := etherTxDone
        }
      }
    }
    // ========================================================================
    // etherTxDone: Transmission complete, return to idle
    // ========================================================================
    is (etherTxDone) {
      etherTxReady := true.B
      etherTxState := etherTxIdle
    }
  }

  // ========================================================================
  // RX Packet Reception Logic
  // ========================================================================
  // Handle ethernet RX and add packets to RX queue
  // Connect RX queue input

  // MASSIVE LUT REDUCTION: Use single register instead of Vec
  // Removes Cat() operations
  val rxPacketReg = RegInit(0.U(TLOE_PACKET_SIZE.W))  // Accumulated RX packet (880 bits)
  val rxCount = RegInit(0.U(8.W))                     // Count received chunks (need 8 bits for 68 chunks)
  val rxFlitSize = RegInit(0.U(8.W))                  // Total flit size (need 8 bits for 68 chunks)
  val rxPacketComplete = RegInit(false.B)              // Packet reception complete flag

  // Debug registers removed to save LUTs

  // ========================================================================
  // RX Packet Accumulation
  // ========================================================================
  // Reset flags when no valid RX data
  when(!io.rxvalid) {
    rxPacketComplete := false.B
  }

  // Process incoming ethernet packet - direct operation, no Cat()
  when(io.rxvalid) {
    val endianSwappedData = TloePacGen.toBigEndian512(io.rxdata)  // Convert from big-endian
    // Calculate shift amount to place data in correct position
    // MAX_CHUNKS = 14, so we shift from top down
    val shiftAmount = ((TLOEEtherQSFP1Constants.MAX_CHUNKS.U - rxCount - 1.U) * 64.U) - 16.U
    
    // Direct accumulation without Vec conversion
    rxPacketReg := rxPacketReg | (endianSwappedData << shiftAmount)
    rxCount := rxCount + 1.U
    
    when(io.rxlast) {
      rxFlitSize := rxCount + 1.U  // Total chunks received
      rxPacketComplete := true.B
      rxCount := 0.U
    }
  }
  
  // ========================================================================
  // RX Queue Enqueue Logic
  // ========================================================================
  // When packet is complete, add to RX queue - direct access
  when(rxPacketComplete && rxQueue.io.enq.ready) {
    // Check ethernet type field (16 bits after srcMAC and destMAC)
    // Packet structure: [srcMAC(48)][destMAC(48)][etherType(16)][data...]
    // etherType is at bits TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE + 15 down to TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE
    val etherTypeStartBit = TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE  // = 880 - 112 = 768
    val etherTypeEndBit = TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE + 15  // = 880 - 112 + 15 = 783
    when(rxPacketReg(etherTypeEndBit, etherTypeStartBit) === ETHER_TYPE) {
      // Valid TLOE packet - enqueue to RX queue
      rxQueue.io.enq.valid := true.B
      // Extract frame data (skip ethernet header: 112 bits = 14 bytes)
      // Frame data starts at TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE = TLOE_PACKET_SIZE - 112
      rxQueue.io.enq.bits.data := rxPacketReg(TLOE_FRAME_SIZE - 1, 0)
      rxQueue.io.enq.bits.flitSize := rxFlitSize - 2.U  // Subtract 2 for MAC headers
    }.otherwise {
      // Invalid ethernet type - discard packet
      rxQueue.io.enq.valid := false.B
    }
    
    rxPacketComplete := false.B
    rxPacketReg := 0.U
  }.otherwise {
    rxQueue.io.enq.valid := false.B
  }
  
  // ========================================================================
  // RX Queue Dequeue Logic
  // ========================================================================
  // Process RX queue and send packets to endpoint
  when(rxQueue.io.deq.valid && io.endpointRxReady) {
    val rxEntry = rxQueue.io.deq.bits
    
    // Send packet data to endpoint
    io.endpointRxValid := true.B
    io.endpointRxData := rxEntry.data
    io.endpointRxFlitSize := rxEntry.flitSize
    
    // Dequeue from queue only when endpoint is ready
    rxQueue.io.deq.ready := true.B
  }.otherwise {
    // No valid RX data from queue or endpoint not ready
    io.endpointRxData := 0.U
    io.endpointRxValid := false.B
    io.endpointRxFlitSize := 0.U
    rxQueue.io.deq.ready := false.B
  }

  //////////////////////////////////////////////////////////////////
  // Debug
  //////////////////////////////////////////////////////////////////
} 
