package omnixtend

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

import OmniXtendConstants._

/**
 * TLOEReceiver Module
 * 
 * This module handles the reception and processing of TLOE packets.
 * It manages:
 * - Packet validation and sequence number checking
 * - Flow control credit management
 * - Retransmission requests (NAK handling)
 * - ACK generation and forwarding to transmitter
 * - TileLink message extraction and forwarding
 * 
 * Key features:
 * - Detects duplicate, out-of-sequence, and normal packets
 * - Manages sliding window for reliable delivery
 * - Extracts TileLink messages from TLOE frames
 */
class TLOEReceiver extends Module {
  val io = IO(new Bundle {
    // ========================================================================
    // Sequence Management - Control RX sequence numbers
    // ========================================================================
    //val incTxSeq = Output(Bool())  // Unused
    val incRxSeq = Output(Bool())        // Increment RX sequence number
    val updateAckSeq = Output(Bool())    // Update acknowledged sequence number
    val newAckSeq = Output(UInt(22.W))   // New ACK sequence number to set

    val nextTxSeq = Input(UInt(22.W))    // Current TX sequence number from seq manager
    val nextRxSeq = Input(UInt(22.W))    // Current RX sequence number from seq manager

    // ========================================================================
    // Flow Control - Credit management
    // ========================================================================
    val incCreditValid = Output(Bool())        // Increment credit valid signal
    val incCreditChannel = Output(UInt(3.W))    // Channel to increment credit
    val incCreditAmount = Output(UInt(5.W))     // Amount of credit to increment

    val incAccCreditValid = Output(Bool())     // Increment accumulated credit valid
    val incAccCreditChannel = Output(UInt(3.W)) // Channel for accumulated credit
    val incAccCreditAmount = Output(UInt(5.W))  // Amount of accumulated credit to increment

    // ========================================================================
    // Slide Window - Sliding window for reliable delivery
    // ========================================================================
    val slideValid = Output(Bool())        // Slide window valid signal
    val slideSeqNumAck = Output(UInt(22.W)) // Sequence number to acknowledge
    val slideDone = Input(Bool())           // Slide window operation complete

    // ========================================================================
    // Retransmission - Request retransmission on NAK
    // ========================================================================
    val retransmitDone = Input(Bool())      // Retransmission operation complete
    val retransmitSeqNum = Output(UInt(22.W)) // Sequence number to retransmit
    val retransmitValid = Output(Bool())    // Retransmission request valid

    // ========================================================================
    // ACK Transfer to Transmitter
    // ========================================================================
    val ackSeqNum = Output(UInt(22.W))    // Acknowledged sequence number
    val ackType = Output(UInt(2.W))        // Type of ACK (ACK/NAK)
    val ackReady = Output(Bool())          // ACK data ready signal
    val ackAckonly = Output(Bool())        // ACK-only packet request
    val ackAckonlyDone = Input(Bool())     // ACK-only packet sent

    // ========================================================================
    // TileLink Handler Interface
    // ========================================================================
    val tlMsg = Output(UInt(4096.W))       // Extracted TileLink message
    val tlMsgMask = Output(UInt(64.W))     // Byte enable mask
    val doTilelinkHandler = Output(Bool())  // Trigger TileLink handler

    // ========================================================================
    // RX Interface - From TLOE Ethernet layer
    // ========================================================================
    val rxFrame = Input(UInt(TLOE_FRAME_SIZE.W))  // Received TLOE frame
    val rxValid = Input(Bool())                    // Frame valid signal
    val rxFlitSize = Input(UInt(7.W))              // Number of flits in frame
    val rxReady = Output(Bool())                   // Ready to receive next frame
  })

  // ========================================================================
  // Default Output Initialization
  // ========================================================================
  // TileLink Handler
  io.tlMsg := 0.U
  io.tlMsgMask := 0.U
  io.doTilelinkHandler := false.B
  
  // ========================================================================
  // RX Ready Signal Management
  // ========================================================================
  // Use register to avoid combinational loop
  val rxReadyReg = RegInit(true.B)
  io.rxReady := rxReadyReg
  
  // ========================================================================
  // State Machine Definition
  // ========================================================================
  // State registers
  val rxIdle :: rxPacketReceived :: rxSlideWindow :: rxRetransmission :: rxAckOnly :: rxCheckType :: rxFrameNormal :: rxFrameDup :: rxFrameOOS :: rxDone :: Nil = Enum(10)
  val rxState = RegInit(rxIdle)

  // ========================================================================
  // Packet Header and Data Registers
  // ========================================================================
  val tloeHeader = Reg(new tloeHeader)      // TLOE header extracted from frame
  val tlHeader = Reg(new TLMessageHigh)     // TileLink header (unused in current implementation)
  val rxFrameMask = RegInit(0.U(64.W))      // Mask extracted from frame

  // ========================================================================
  // TileLink Handler Control
  // ========================================================================
  val do_tilelink_handler = RegInit(false.B)
  io.doTilelinkHandler := do_tilelink_handler

  // ========================================================================
  // Internal Registers
  // ========================================================================
  // Debug registers removed to save LUTs
  val rxRequiredFlits = RegInit(0.U(8.W))  // Required number of flits for TileLink message
  // val incAccCreditValid = RegInit(false.B)  // Removed
  // val incAccCreditChannel = RegInit(0.U(3.W))  // Removed
  // val incAccCreditAmount = RegInit(0.U(8.W))  // Removed

  // ========================================================================
  // Default Output Values
  // ========================================================================
  //io.incTxSeq := false.B  // Unused
  io.incRxSeq := false.B
  io.updateAckSeq := false.B
  io.newAckSeq := 0.U

  io.incCreditValid := false.B
  io.incCreditChannel := 0.U
  io.incCreditAmount := 0.U

  io.incAccCreditValid := false.B
  io.incAccCreditChannel := 0.U
  io.incAccCreditAmount := 0.U

  io.slideValid := false.B
  io.slideSeqNumAck := 0.U

  io.retransmitSeqNum := 0.U
  io.retransmitValid := false.B

  io.ackSeqNum := 0.U
  io.ackType := 0.U
  io.ackReady := false.B

  // ========================================================================
  // ACK-Only Control Register
  // ========================================================================
  // ackAckonly는 레지스터로 관리하여 상태를 유지
  val ackAckonlyReg = RegInit(false.B)
  io.ackAckonly := ackAckonlyReg  

  // ========================================================================
  // Request Type Enumeration
  // ========================================================================
  // Request type enumeration for packet classification
  object ReqType {
    val REQ_NORMAL = 0.U(2.W)      // Normal packet (expected sequence number)
    val REQ_DUPLICATE = 1.U(2.W)   // Duplicate packet (already received)
    val REQ_OOS = 2.U(2.W)         // Out of Sequence (future sequence number)
  }

  // ========================================================================
  // Helper Functions
  // ========================================================================
  // Function to determine request type based on sequence number comparison
  def getReqType(rxSeq: UInt, nextRxSeq: UInt): UInt = {
    val diff = TLOESeqManager.seqNumCompare(rxSeq, nextRxSeq)
    // Convert SInt to UInt for MuxLookup
    val diffUInt = Wire(UInt(2.W))
    when(diff === 0.S) {
      diffUInt := 0.U  // Normal (expected)
    }.elsewhen(diff === (-1).S) {
      diffUInt := 1.U  // Duplicate (previous)
    }.otherwise {
      diffUInt := 2.U  // Out of sequence (future)
    }

    MuxLookup(diffUInt, ReqType.REQ_OOS)(Seq(
      0.U -> ReqType.REQ_NORMAL,
      1.U -> ReqType.REQ_DUPLICATE,
      2.U -> ReqType.REQ_OOS
    ))
  }

  // ========================================================================
  // ACK-Only Done Handling
  // ========================================================================
  when(io.ackAckonlyDone) {
    ackAckonlyReg := false.B
  } 

  // ========================================================================
  // RX Frame Buffering (Minimal)
  // ========================================================================
  val rxTloeHeader = RegInit(0.U.asTypeOf(new tloeHeader))
  // MASSIVE LUT REDUCTION: Keep only minimal buffering
  // Store only header and mask, process frame directly from input
  val rxFlitSize = RegInit(0.U(7.W))
  val rxFrameInput = Reg(UInt(TLOE_FRAME_SIZE.W))  // Single register instead of Vec

  // ========================================================================
  // RX Frame Reception Logic
  // ========================================================================
  when (io.rxValid) {
    rxReadyReg := false.B
    // Extract TLOE header from top of frame (64 bits)
    rxTloeHeader := io.rxFrame(TLOE_FRAME_SIZE-1, TLOE_FRAME_SIZE-64).asTypeOf(new tloeHeader)
    rxFrameInput := io.rxFrame  // Simple register assignment
    rxFlitSize := io.rxFlitSize
    rxState := rxPacketReceived
  }

  // ========================================================================
  // RX State Machine Logic
  // ========================================================================
  // State machine for packet reception
  switch(rxState) {
    // ========================================================================
    // rxIdle: Wait for incoming packet
    // ========================================================================
    is(rxIdle) {
    }

    // ========================================================================
    // rxPacketReceived: Extract mask from received frame
    // ========================================================================
    is(rxPacketReceived) {
      // Extract mask directly from frame - no Vec indexing needed
      // Mask is at the last flit position
      val maskBitPos = (rxFlitSize - 1.U) * 64.U
      rxFrameMask := (rxFrameInput >> maskBitPos)(63, 0)
      rxState := rxSlideWindow
    }

    // ========================================================================
    // rxSlideWindow: Update sliding window with acknowledged sequence number
    // ========================================================================
    is(rxSlideWindow) {
      // Serve Ack - update sliding window
      io.slideValid := true.B
      io.slideSeqNumAck := rxTloeHeader.seqNumAck

      when (io.slideDone) {
        rxState := rxRetransmission
      }
    }

    // ========================================================================
    // rxRetransmission: Handle NAK and request retransmission
    // ========================================================================
    is(rxRetransmission) {
      // In case of NAK, retransmit the frame in the retransmit buffer
      when (rxTloeHeader.ack === TLOE_NAK) {
        io.retransmitValid := true.B
        io.retransmitSeqNum := rxTloeHeader.seqNumAck
      }.otherwise {
        rxState := rxAckOnly
      }

      when (io.retransmitDone) {
        rxState := rxAckOnly
      }
    }

    // ========================================================================
    // rxAckOnly: Handle ACK-only packets
    // ========================================================================
    is(rxAckOnly) {
      when(rxTloeHeader.msgType === TLOE_TYPE_ACKONLY && rxTloeHeader.ack === TLOE_ACK) {
        // ACK-only packet - just update acknowledged sequence number
        io.newAckSeq := rxTloeHeader.seqNumAck
        io.updateAckSeq := true.B  // Set updateAckSeq when ackdSeq is updated
        rxState := rxDone
      }.otherwise {
        rxState := rxCheckType  // Not ACK-only, check packet type
      }
    }

    // ========================================================================
    // rxCheckType: Classify packet type (normal, duplicate, out-of-sequence)
    // ========================================================================
    is(rxCheckType) {
      val reqType = getReqType(rxTloeHeader.seqNum, io.nextRxSeq)

      switch(reqType) {
        is(ReqType.REQ_NORMAL) {
          rxState := rxFrameNormal  // Expected sequence number
        }
        is(ReqType.REQ_DUPLICATE) {
          rxState := rxFrameDup  // Already received
        }
        is(ReqType.REQ_OOS) {
          rxState := rxFrameOOS  // Future sequence number
        }
      }
    }

    // ========================================================================
    // rxFrameNormal: Process normal (expected) packet
    // ========================================================================
    is(rxFrameNormal) {
      // Update sequence numbers
      io.incRxSeq := true.B
      io.newAckSeq := rxTloeHeader.seqNumAck
      io.updateAckSeq := true.B

      // Handle non-zero mask case - direct access, no Cat()
      when(rxFrameMask =/= 0.U) {
        // Extract TileLink message (between TLOE header and mask)
        io.tlMsg := rxFrameInput(TLOE_FRAME_SIZE-65, 64)
        io.tlMsgMask := rxFrameMask
        io.doTilelinkHandler := true.B
        rxRequiredFlits := TlMsgFlits.getFlitsCnt(tlHeader.chan, tlHeader.opcode, tlHeader.size)
      }.otherwise {
        // Zero mask - request ACK-only packet
        ackAckonlyReg := true.B
      }

      // Handle both credits in one state to save LUTs
      when(rxTloeHeader.chan =/= CHANNEL_0) {
        // Increment credit for non-zero channel
        io.incCreditChannel := rxTloeHeader.chan
        io.incCreditAmount := (1.U << rxTloeHeader.credit)
        io.incCreditValid := true.B
      }
      
      when(rxFrameMask =/= 0.U) {
        // Increment accumulated credit when data is present
        io.incAccCreditChannel := rxTloeHeader.chan
        io.incAccCreditAmount := rxRequiredFlits
        io.incAccCreditValid := true.B
      }

      rxState := rxDone
    }

    // ========================================================================
    // rxFrameDup: Handle duplicate packet
    // ========================================================================
    is(rxFrameDup) {
      // TODO: tx에 Ackonly Frame 전송하도록 요청
      // Send ACK for duplicate packet
      io.ackSeqNum := rxTloeHeader.seqNumAck
      io.ackType := TLOE_ACK
      io.ackReady := true.B

      rxState := rxDone
    }

    // ========================================================================
    // rxFrameOOS: Handle out-of-sequence packet
    // ========================================================================
    is(rxFrameOOS) {
      // TODO: tx에 Ackonly Frame 전송하도록 요청
      // Send NAK for out-of-sequence packet
      io.ackSeqNum := TLOESeqManager.getPrevSeq(io.nextRxSeq)
      io.ackType := TLOE_NAK
      io.ackReady := true.B

      rxState := rxDone
    }

    // ========================================================================
    // rxDone: Packet processing complete, return to idle
    // ========================================================================
    is(rxDone) {
      rxState := rxIdle
      rxReadyReg := true.B  // Ready for next packet
    }
  }

  //////////////////////////////////////////////////////////////
  // DEBUG - removed to save LUTs
  //////////////////////////////////////////////////////////////
} 
