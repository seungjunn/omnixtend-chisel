package omnixtend

import chisel3._
import chisel3.util._
import chisel3.dontTouch

import TLOEEtherQSFP1Constants._
import OmniXtendConstants._

// TX Queue Entry structure
class TLOETxQueueEntry extends Bundle {
  //val data = UInt(TLOE_PACKET_SIZE.W)
  val data = UInt(TLOE_FRAME_SIZE.W)
  val flitSize = UInt(7.W)
}

// RX Queue Entry structure  
class TLOERxQueueEntry extends Bundle {
  val data = UInt(TLOE_PACKET_SIZE.W)
  val flitSize = UInt(7.W)
}

object TLOEEtherQSFP1Constants {
  // Queue configuration
  val TX_QUEUE_DEPTH = 2  // Number of packets that can be queued for transmission
  val RX_QUEUE_DEPTH = 2  // Number of packets that can be queued for reception
  
  // Packet size configuration
  val MAX_CHUNKS = (TLOE_PACKET_SIZE+16) / 64  // = (880+16) / 64 = 896 / 64 = 14 chunks

  val MAC_ADDR_01 = "h001232FFFFFC".U(48.W)
  val MAC_ADDR_02 = "h001232FFFFFA".U(48.W)

  val ETHER_TYPE = 0xaaaa.U(16.W)
}

class TLOEEtherQSFP1 extends Module {
  val io = IO(new Bundle {
    // Ethernet Interface - External connection
    val txdata = Output(UInt(64.W))  // Changed to 64-bit for actual ethernet transmission
    val txvalid = Output(Bool())
    val txlast = Output(Bool())
    val txkeep = Output(UInt(8.W))
    val txready = Input(Bool())

    // Internal TX Interface for other modules
    val internalTxData = Input(UInt(TLOE_FRAME_SIZE.W))
    val internalTxFlitSize = Input(UInt(7.W))
    val internalTxValid = Input(Bool())
    val internalTxReady = Output(Bool())

     // Received Packet
    val rxdata = Input(UInt(512.W))
    val rxvalid = Input(Bool())
    val rxlast = Input(Bool())
   
    // RX interface to endpoint
    val endpointRxData = Output(UInt(TLOE_FRAME_SIZE.W))
    val endpointRxValid = Output(Bool())
    val endpointRxFlitSize = Output(UInt(7.W))
    val endpointRxReady = Input(Bool())  // Endpoint ready to receive packet

    val modeConn = Input(UInt(2.W))
  })

  val srcMac = RegInit(0.U(48.W))
  val destMac = RegInit(0.U(48.W))
  val etherType = RegInit(0.U(16.W))

  val modeConn = RegInit(0.U(2.W))
  modeConn := io.modeConn

  // Initialize MAC address and etherType
  val isMacSet = RegInit(false.B)
  when (!isMacSet && modeConn =/= MODE_IDLE) {
    when (modeConn === MODE_MASTER) {
      srcMac := MAC_ADDR_02
      destMac := MAC_ADDR_01
    }.otherwise {
      srcMac := MAC_ADDR_01
      destMac := MAC_ADDR_02
    }
    etherType := ETHER_TYPE

    isMacSet := true.B
  }

  // TX/RX Queue modules using Chisel Queue
  val txQueue = Module(new Queue(new TLOETxQueueEntry, TLOEEtherQSFP1Constants.TX_QUEUE_DEPTH, pipe=false, flow=false))
  val rxQueue = Module(new Queue(new TLOERxQueueEntry, TLOEEtherQSFP1Constants.RX_QUEUE_DEPTH, pipe=false, flow=false))

  // Internal TX interface registers
  val internalTxIdx = RegInit(0.U(4.W))
  val internalTxReady = RegInit(true.B)

  // MASSIVE LUT REDUCTION: Use single register instead of Vec
  // Removes Cat() operations that consume huge LUTs
  // Add 16 bits padding to align to 64-bit boundary (880 + 16 = 896 = 14 * 64)
  val etherTloePacket = Reg(UInt((TLOE_PACKET_SIZE + 16).W))
  //val internalTxDataReg = RegInit(0.U(TLOE_FRAME_SIZE.W))
  val internalTxFlitSizeReg = RegInit(0.U(7.W))
  val internalTxReadyReg = RegInit(true.B)
  
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

  //////////////////////////////////////////////////////////////////
  // Handle incoming packets and add to TX queue
  // Input Packet to TX queue
  txQueue.io.enq.valid := io.internalTxValid
  txQueue.io.enq.bits.data := io.internalTxData
  txQueue.io.enq.bits.flitSize := io.internalTxFlitSize

  val etherTxIdle :: etherTxPrepare :: etherTxSend :: etherTxDone :: Nil = Enum(4)
  val etherTxState = RegInit(etherTxIdle)
  
  val etherTxReady = RegInit(true.B)

  when (txQueue.io.deq.valid && etherTxReady) {
    etherTxReady := false.B
    etherTxState := etherTxPrepare
  }

  switch (etherTxState) {
    is (etherTxIdle) {
    }

    is (etherTxPrepare) {
      val txEntry = txQueue.io.deq.bits

      internalTxIdx := 0.U
      // Add 16-bit padding to align to 64-bit boundary
      etherTloePacket := Cat(srcMac, destMac, etherType, txEntry.data, 0.U(16.W))
      internalTxFlitSizeReg := Mux(txEntry.flitSize < 4.U, 4.U, txEntry.flitSize)

      txQueue.io.deq.ready := true.B
      etherTxState := etherTxSend
    }

    is (etherTxSend) {
      // +4 is for the TloeHeader and mask
      val flitSize = internalTxFlitSizeReg + 4.U

when (io.txready === true.B) {

      when(internalTxIdx < flitSize) {
        // Extract 64-bit chunk - direct access, no Cat()
        // Packet is now 896 bits (TLOE_PACKET_SIZE + 16) to align to 64-bit boundary
        val bitPosition = (internalTxIdx * 64.U)
        val packetSize = (TLOE_PACKET_SIZE + 16).U
        val chunk64 = (etherTloePacket >> (packetSize - 64.U - bitPosition))(63, 0)

        io.txvalid := true.B
        io.txdata := TloePacGen.toBigEndian(chunk64)
        
        // Check if this is the last chunk
        when(internalTxIdx === (flitSize - 1.U)) {
          io.txlast := true.B
          io.txkeep := "h3F".U
        }.otherwise {
          io.txlast := false.B
          io.txkeep := "hFF".U
        }

        internalTxIdx := internalTxIdx + 1.U
      }.elsewhen (internalTxIdx >= flitSize) {
        io.txdata := 0.U
        io.txvalid := false.B
        io.txlast := false.B
        io.txkeep := 0.U 

        internalTxIdx := 0.U
        etherTxState := etherTxDone
      }
    }
}
    is (etherTxDone) {
      etherTxReady := true.B

      etherTxState := etherTxIdle
    }
  }

  //////////////////////////////////////////////////////////////////
  // Handle ethernet RX and add packets to RX queue
  // Connect RX queue input

  // MASSIVE LUT REDUCTION: Use single register instead of Vec
  // Removes Cat() operations
  val rxPacketReg = RegInit(0.U(TLOE_PACKET_SIZE.W))
  val rxCount = RegInit(0.U(8.W))  // Count received chunks (need 8 bits for 68 chunks)
  val rxFlitSize = RegInit(0.U(8.W))  // Total flit size (need 8 bits for 68 chunks)
  val rxPacketComplete = RegInit(false.B)  // Packet reception complete flag

  // Debug registers removed to save LUTs

  // Reset flags when no valid RX data
  when(!io.rxvalid) {
    rxPacketComplete := false.B
  }

  // Process incoming ethernet packet - direct operation, no Cat()
  when(io.rxvalid) {
    val endianSwappedData = TloePacGen.toBigEndian512(io.rxdata)
    val shiftAmount = ((TLOEEtherQSFP1Constants.MAX_CHUNKS.U - rxCount - 1.U) * 64.U) - 16.U
    
    // Direct accumulation without Vec conversion
    rxPacketReg := rxPacketReg | (endianSwappedData << shiftAmount)
    rxCount := rxCount + 1.U
    
    when(io.rxlast) {
      rxFlitSize := rxCount + 1.U
      rxPacketComplete := true.B
      rxCount := 0.U
    }
  }
  
  // When packet is complete, add to RX queue - direct access
  when(rxPacketComplete && rxQueue.io.enq.ready) {
    // Check ethernet type field (16 bits after srcMAC and destMAC)
    // Packet structure: [srcMAC(48)][destMAC(48)][etherType(16)][data...]
    // etherType is at bits TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE + 15 down to TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE
    val etherTypeStartBit = TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE  // = 880 - 112 = 768
    val etherTypeEndBit = TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE + 15  // = 880 - 112 + 15 = 783
    when(rxPacketReg(etherTypeEndBit, etherTypeStartBit) === ETHER_TYPE) {
      rxQueue.io.enq.valid := true.B
      // Extract frame data (skip ethernet header: 112 bits = 14 bytes)
      // Frame data starts at TLOE_PACKET_SIZE - TLOE_ETHER_HEADER_SIZE = TLOE_PACKET_SIZE - 112
      rxQueue.io.enq.bits.data := rxPacketReg(TLOE_FRAME_SIZE - 1, 0)
      rxQueue.io.enq.bits.flitSize := rxFlitSize - 2.U
    }.otherwise {
      rxQueue.io.enq.valid := false.B
    }
    
    rxPacketComplete := false.B
    rxPacketReg := 0.U
  }.otherwise {
    rxQueue.io.enq.valid := false.B
  }
  
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
