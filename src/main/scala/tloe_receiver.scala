package omnixtend

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

import OmniXtendConstants._

class TLOEReceiver extends Module {
  val io = IO(new Bundle {
    // Sequence Management
    //val incTxSeq = Output(Bool())
    val incRxSeq = Output(Bool())
    val updateAckSeq = Output(Bool())
    val newAckSeq = Output(UInt(22.W))

    val nextTxSeq = Input(UInt(22.W))
    val nextRxSeq = Input(UInt(22.W))

    // Flow Control
    val incCreditValid = Output(Bool())
    val incCreditChannel = Output(UInt(3.W))
    val incCreditAmount = Output(UInt(5.W))

    val incAccCreditValid = Output(Bool())
    val incAccCreditChannel = Output(UInt(3.W))
    val incAccCreditAmount = Output(UInt(5.W))

/*
    // Slide Window
    val slideValid = Output(Bool())
    val slideSeqNumAck = Output(UInt(22.W))
    val slideDone = Input(Bool()) // TODO Ready??

    // Retransmission
    val retransmitDone = Input(Bool()) // TODO Ready??
    val retransmitSeqNum = Output(UInt(22.W))
    val retransmitValid = Output(Bool())
    */

    // Transfer ack info to Tx
    val ackSeqNum = Output(UInt(22.W))
    val ackType = Output(UInt(2.W))
    val ackReady = Output(Bool())
    val ackAckonly = Output(Bool())
    val ackAckonlyDone = Input(Bool())

    // TileLink Handler
    val tlMsg = Output(UInt(4096.W))
    val tlMsgMask = Output(UInt(64.W))
    val doTilelinkHandler = Output(Bool())

    // RX interface for receiving packets from TLOEEther
    val rxFrame = Input(UInt(TLOE_FRAME_SIZE.W))
    val rxValid = Input(Bool())
    val rxFlitSize = Input(UInt(7.W))
    val rxReady = Output(Bool())
  })

  // TileLink Handler
  io.tlMsg := 0.U
  io.tlMsgMask := 0.U
  io.doTilelinkHandler := false.B
  
  // Use register to avoid combinational loop
  val rxReadyReg = RegInit(true.B)
  io.rxReady := rxReadyReg
  
  // State registers - Reduced from 12 to 7 states to save LUTs
  // Merged: rxSlideWindow+rxRetransmission+rxAckOnly -> rxCheckType
  // Merged: rxHandleCredit+rxHandleAccCredit -> rxHandleCredits
  val rxIdle :: rxPacketReceived :: rxCheckType :: rxFrameNormal :: rxFrameDup :: rxFrameOOS :: rxDone :: Nil = Enum(7)
  val rxState = RegInit(rxIdle)

  val tloeHeader = Reg(new tloeHeader)
  val tlHeader = Reg(new TLMessageHigh)
  val rxFrameMask = RegInit(0.U(64.W))

  val do_tilelink_handler = RegInit(false.B)
  io.doTilelinkHandler := do_tilelink_handler

  // Debug registers removed to save LUTs
  val rxRequiredFlits = RegInit(0.U(8.W))
  // val incAccCreditValid = RegInit(false.B)
  // val incAccCreditChannel = RegInit(0.U(3.W))
  // val incAccCreditAmount = RegInit(0.U(8.W))

  //io.incTxSeq := false.B
  io.incRxSeq := false.B
  io.updateAckSeq := false.B
  io.newAckSeq := 0.U

  io.incCreditValid := false.B
  io.incCreditChannel := 0.U
  io.incCreditAmount := 0.U

  io.incAccCreditValid := false.B
  io.incAccCreditChannel := 0.U
  io.incAccCreditAmount := 0.U

/*
  io.slideValid := false.B
  io.slideSeqNumAck := 0.U

  io.retransmitSeqNum := 0.U
  io.retransmitValid := false.B
  */

  io.ackSeqNum := 0.U
  io.ackType := 0.U
  io.ackReady := false.B

  // ackAckonly는 레지스터로 관리하여 상태를 유지
  val ackAckonlyReg = RegInit(false.B)
  io.ackAckonly := ackAckonlyReg  

  // Request type enumeration
  object ReqType {
    val REQ_NORMAL = 0.U(2.W)
    val REQ_DUPLICATE = 1.U(2.W)
    val REQ_OOS = 2.U(2.W)  // Out of Sequence
  }

  // Function to determine request type based on sequence number comparison
  def getReqType(rxSeq: UInt, nextRxSeq: UInt): UInt = {
    val diff = TLOESeqManager.seqNumCompare(rxSeq, nextRxSeq)
    // Convert SInt to UInt for MuxLookup
    val diffUInt = Wire(UInt(2.W))
    when(diff === 0.S) {
      diffUInt := 0.U
    }.elsewhen(diff === (-1).S) {
      diffUInt := 1.U
    }.otherwise {
      diffUInt := 2.U
    }

    MuxLookup(diffUInt, ReqType.REQ_OOS)(Seq(
      0.U -> ReqType.REQ_NORMAL,
      1.U -> ReqType.REQ_DUPLICATE,
      2.U -> ReqType.REQ_OOS
    ))
  }

  when(io.ackAckonlyDone) {
    ackAckonlyReg := false.B
  } 

  val rxTloeHeader = RegInit(0.U.asTypeOf(new tloeHeader))
  // MASSIVE LUT REDUCTION: Keep only minimal buffering
  // Store only header and mask, process frame directly from input
  val rxFlitSize = RegInit(0.U(7.W))
  val rxFrameInput = Reg(UInt(TLOE_FRAME_SIZE.W))  // Single register instead of Vec

  // Debug registers removed to save LUTs
  val rx_debug_rxState = RegInit(0.U(8.W))
  rx_debug_rxState := rxState

  val rx_debug_rxFrame = RegInit(0.U(512.W))
  rx_debug_rxFrame := rxFrameInput(767, 256)

  when (io.rxValid) {
    rxReadyReg := false.B
    // Extract TLOE header from top of frame (64 bits)
    rxTloeHeader := io.rxFrame(TLOE_FRAME_SIZE-1, TLOE_FRAME_SIZE-64).asTypeOf(new tloeHeader)
    rxFrameInput := io.rxFrame  // Simple register assignment
    rxFlitSize := io.rxFlitSize
    rxState := rxPacketReceived
  }

  // State machine for packet reception
  switch(rxState) {
    is(rxIdle) {
    }

    is(rxPacketReceived) {
      // Extract mask directly from frame - no Vec indexing needed
      val maskBitPos = (rxFlitSize - 1.U) * 64.U
      rxFrameMask := (rxFrameInput >> maskBitPos)(63, 0)
      rxState := rxCheckType
    }

    is(rxCheckType) {
      val reqType = getReqType(rxTloeHeader.seqNum, io.nextRxSeq)

      switch(reqType) {
        is(ReqType.REQ_NORMAL) {
          rxState := rxFrameNormal
        }
        is(ReqType.REQ_DUPLICATE) {
          rxState := rxFrameDup
        }
        is(ReqType.REQ_OOS) {
          rxState := rxFrameOOS
        }
      }

      rxState := rxFrameNormal
    }

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
        ackAckonlyReg := true.B
      }

      // Handle both credits in one state to save LUTs
      when(rxTloeHeader.chan =/= CHANNEL_0) {
        io.incCreditChannel := rxTloeHeader.chan
        io.incCreditAmount := (1.U << rxTloeHeader.credit)
        io.incCreditValid := true.B
      }
      
      when(rxFrameMask =/= 0.U) {
        io.incAccCreditChannel := rxTloeHeader.chan
        io.incAccCreditAmount := rxRequiredFlits
        io.incAccCreditValid := true.B
      }

      rxState := rxDone
    }

    is(rxFrameDup) {
      // TODO tx에 Ackonly Frame 전송하도록 요청
      io.ackSeqNum := rxTloeHeader.seqNumAck
      io.ackType := TLOE_ACK
      io.ackReady := true.B

      rxState := rxDone
    }

    is(rxFrameOOS) {
      // TODO tx에 Ackonly Frame 전송하도록 요청
      io.ackSeqNum := TLOESeqManager.getPrevSeq(io.nextRxSeq)
      io.ackType := TLOE_NAK
      io.ackReady := true.B

      rxState := rxDone
    }

    is(rxDone) {
      rxState := rxIdle
      rxReadyReg := true.B
    }
  }

  //////////////////////////////////////////////////////////////
  // DEBUG - removed to save LUTs
  //////////////////////////////////////////////////////////////
  dontTouch(rx_debug_rxState)
  dontTouch(rx_debug_rxFrame)
  /*
  dontTouch(rxState)

  dontTouch(tloeHeader)
  dontTouch(tlHeader)
  dontTouch(rxFrameMask)
  dontTouch(do_tilelink_handler)
  dontTouch(rxRequiredFlits)
  dontTouch(incAccCreditValid)
  dontTouch(incAccCreditChannel)
  dontTouch(incAccCreditAmount)

  dontTouch(rxReadyReg)
  dontTouch(ackAckonlyReg)
  */
} 
