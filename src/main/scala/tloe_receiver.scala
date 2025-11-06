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
  
  // State registers
  val rxIdle :: rxPacketReceived :: rxSlideWindow :: rxRetransmission :: rxAckOnly :: rxCheckType :: rxFrameNormal :: rxHandleCredit :: rxFrameDup :: rxFrameOOS :: rxHandleAccCredit :: rxDone :: Nil = Enum(12)
  val rxState = RegInit(rxIdle)

  val tloeHeader = Reg(new tloeHeader)
  val tlHeader = Reg(new TLMessageHigh)
  val rxFrameMask = RegInit(0.U(64.W))

  val do_tilelink_handler = RegInit(false.B)
  io.doTilelinkHandler := do_tilelink_handler

  // debug
  val rxRequiredFlits = RegInit(0.U(8.W))
  val incAccCreditValid = RegInit(false.B)
  val incAccCreditChannel = RegInit(0.U(3.W))
  val incAccCreditAmount = RegInit(0.U(8.W))

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
  val rxFrame = RegInit(0.U(TLOE_FRAME_SIZE.W))
  val rxFlitSize = RegInit(0.U(7.W))

  when (io.rxValid) {
    rxReadyReg := false.B

    rxTloeHeader := io.rxFrame(4223, 4160).asTypeOf(new tloeHeader)
    rxFrame := io.rxFrame
    rxFlitSize := io.rxFlitSize

    rxState := rxPacketReceived
  }

  // State machine for packet reception
  switch(rxState) {
    is(rxIdle) {
    }

    is(rxPacketReceived) {
      // Extract mask from frame: Mask는 flitSize 번째 flit의 마지막 64비트에 위치
      rxFrameMask := (rxFrame >> (TLOE_FRAME_SIZE.U - (rxFlitSize * 64.U)))(63, 0)

      rxState := rxSlideWindow
    }

    is(rxSlideWindow) {
        /*
      // Serve Ack
      io.slideValid := true.B
      io.slideSeqNumAck := rxTloeHeader.seqNumAck

      when (io.slideDone) {
        rxState := rxRetransmission
      }
      */
    }

    is(rxRetransmission) {
        /*
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
      */
    }

    is(rxAckOnly) {
      when(rxTloeHeader.msgType === TLOE_TYPE_ACKONLY && rxTloeHeader.ack === TLOE_ACK) {
        io.newAckSeq := rxTloeHeader.seqNumAck
        io.updateAckSeq := true.B  // Set updateAckSeq when ackdSeq is updated
        rxState := rxDone
      }.otherwise {
        rxState := rxCheckType
      }
  
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
    }

    is(rxFrameNormal) {
      when(rxFrameMask === 0.U) {
        // Zero-tl frame
        io.incRxSeq := true.B  // Set incRxSeq when incrementing RX sequence
        io.newAckSeq := rxTloeHeader.seqNumAck
        io.updateAckSeq := true.B

        rxState := rxHandleCredit
        
        // TODO tx에 Ackonly Frame 전송하도록 요청
        ackAckonlyReg := true.B
      }.otherwise {
        // Update nextRxSeq
        io.incRxSeq := true.B
        io.newAckSeq := rxTloeHeader.seqNumAck
        io.updateAckSeq := true.B

        io.tlMsg := rxFrame(4159, 64)
        io.tlMsgMask := rxFrameMask
        io.doTilelinkHandler := true.B

        rxRequiredFlits := TlMsgFlits.getFlitsCnt(tlHeader.chan, tlHeader.opcode, tlHeader.size)

        rxState := rxHandleCredit
      }
    }

    is(rxHandleCredit) {
      // TODO check if chan is 0
      when(rxTloeHeader.chan =/= CHANNEL_0) {
        io.incCreditChannel := rxTloeHeader.chan
        io.incCreditAmount := (1.U << rxTloeHeader.credit)
        io.incCreditValid := true.B
      }

      when(rxFrameMask === 0.U) {
        rxState := rxDone
      }.otherwise {
        rxState := rxHandleAccCredit
      }
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

    is(rxHandleAccCredit) {
      io.incAccCreditChannel := rxTloeHeader.chan
      io.incAccCreditAmount := rxRequiredFlits
      io.incAccCreditValid := true.B

      rxState := rxDone
    }

    is(rxDone) {
      rxState := rxIdle
      rxReadyReg := true.B
    }
  }

  when(rxState === rxDone) {
    rxState := rxIdle
  }

  //////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////
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
