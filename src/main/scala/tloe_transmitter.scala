package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._
import TloePacGen._

class TLOETransmitter extends Module {
  val io = IO(new Bundle {
    // TileLink Interface
    val tlChan = Input(UInt(3.W))
    val tlOpcode = Input(UInt(3.W))
    val tlParam = Input(UInt(4.W))
    val tlSize = Input(UInt(4.W))
    val tlSource = Input(UInt(26.W))
    val tlAddr = Input(UInt(64.W))
    val tlData = Input(UInt(512.W))
    val tlMask = Input(UInt(64.W))
    val tlValid = Input(Bool())

    // TLOEEther Interface (simplified)
    val txData = Output(UInt(TLOE_FRAME_SIZE.W))
    val txFlitSize = Output(UInt(7.W))
    val txStart = Output(Bool())
    val txReady = Input(Bool())

    // Sequence Management
    val incTxSeq = Output(Bool())
    val incRxSeq = Output(Bool())
    val updateAckSeq = Output(Bool())
    val newAckSeq = Output(UInt(22.W))

    val nextTxSeq = Input(UInt(22.W))
    val nextRxSeq = Input(UInt(22.W))
    val ackdSeq = Input(UInt(22.W))

    // Flow Control
    val decCreditValid = Output(Bool())
    val decCreditChannel = Output(UInt(3.W))
    val decCreditAmount = Output(UInt(16.W))

    val decAccCreditValid = Output(Bool())
    val decAccCreditChannel = Output(UInt(3.W))
    val decAccCreditAmount = Output(UInt(16.W))

    val credits = Input(Vec(6, UInt(16.W)))
    //val accCredits = Input(Vec(6, UInt(16.W)))
    val maxCreditChannel = Input(UInt(3.W))
    //val error = Input(Bool())
    val maxCredit = Input(UInt(16.W))

/*
    // Retransmission
    val retransmitWrite = Output(new RetransmitBufferElement)
    val retransmitWriteValid = Output(Bool())
    val retransmitIsFull = Input(Bool())

    val isRetransmit = Input(Bool())
    */

    // Timer
    val currTime = Input(UInt(64.W))

    //
    val ackSeqNum = Input(UInt(22.W))
    val ackType = Input(UInt(2.W))
    val ackReady = Input(Bool())    
    val ackAckonly = Input(Bool())
    val ackAckonlyDone = Output(Bool())

    val epConn = Input(Bool())
    val modeConn = Input(UInt(2.W))  // Connection mode: 1=master, 2=slave

    val debug1 = Input(Bool())
    val debug2 = Input(Bool())
  })

  // Initialize TLOEEther interface
  io.txData := 0.U
  io.txStart := false.B
  io.txFlitSize := 0.U

  io.incTxSeq := false.B
  io.incRxSeq := false.B
  io.updateAckSeq := false.B
  io.newAckSeq := 0.U

  io.decCreditValid := false.B
  io.decCreditChannel := 0.U
  io.decCreditAmount := 0.U

  io.decAccCreditValid := false.B
  io.decAccCreditChannel := 0.U
  io.decAccCreditAmount := 0.U

/*
  io.retransmitWrite.tloeFrame := 0.U(TLOE_FRAME_SIZE.W)
  io.retransmitWrite.tloeFrameSize := 0.U(5.W)
  io.retransmitWrite.state := 0.U(2.W)
  io.retransmitWrite.sendTime := 0.U(64.W)
  io.retransmitWriteValid := false.B
  */

  io.ackAckonlyDone := false.B

  val epConn = RegInit(false.B)
  epConn := io.epConn

  val txIdle :: txAckOnly :: txCheckFrame :: txCheckAck :: txCheckCredit :: txInitFrame :: txHandleCredit :: txHandleAccCredit :: txPrepareSend :: txSendPacket :: txEnqRetransmit :: txDone :: Nil = Enum(12)
  val txState = RegInit(txIdle)

  val aidle :: amakeFrame :: asendRequest :: adone :: Nil = Enum(4)
  val astate = RegInit(aidle)

  val tx_size = RegInit(0.U(3.W))

  val nextChan = RegInit(0.U(3.W))
  val nextOpcode = RegInit(0.U(3.W))
  val nextParam = RegInit(0.U(4.W))
  val nextSize = RegInit(0.U(4.W))
  val nextSource = RegInit(0.U(26.W))
  val nextAddr = RegInit(0.U(64.W))
  val nextData = RegInit(0.U(512.W))

  val isFrame = RegInit(false.B)
  val isACK = RegInit(false.B)
  val isCredit = RegInit(false.B)

  val maxAccChannel = RegInit(0.U(3.W))
  val maxAccCredit = RegInit(0.U(16.W))
  maxAccChannel := io.maxCreditChannel
  maxAccCredit := io.maxCredit

  // Register for storing read (rPacket) and write (wPacket) packets
  //val txPacket = dontTouch(RegInit(0.U(896.W)))
  val txFrame = RegInit(0.U(TLOE_FRAME_SIZE.W))
  val txFrameSize = RegInit(0.U(5.W))
  val txRequiredFlits = RegInit(0.U(8.W))

  // TODO delete
  val nAckPacket = RegInit(0.U(TLOE_FRAME_SIZE.W))

  val sendPacket = RegInit(false.B)
  val txComplete = RegInit(true.B)
  val idx = RegInit(0.U(16.W))

  // Registers for AXI-Stream transmission state
  val axi_txdata = RegInit(0.U(64.W))
  val axi_txvalid = RegInit(false.B)
  val axi_txlast = RegInit(false.B)
  val axi_txkeep = RegInit(0.U(8.W))  

  // Prepare data for TLOEEther's internal interface
  // TLOEEther will handle the conversion to ethernet signals
  // Note: TLOEEther is instantiated in OX.scala

   // Single integrated queue for transmission data
  val txQueue = Module(new Queue(new Bundle {
    val chan = UInt(3.W)
    val opcode = UInt(3.W)
    val param = UInt(4.W)
    val size = UInt(4.W)
    val source = UInt(26.W)
    val addr = UInt(64.W)
    val data = UInt(512.W)
  }, 16))

  // Default queue port values
  txQueue.io.enq.valid := false.B
  txQueue.io.enq.bits := 0.U.asTypeOf(txQueue.io.enq.bits)
  txQueue.io.deq.ready := false.B

  val tx_debug_epConn = RegInit(false.B)
  val tx_debug_debug1 = RegInit(false.B)
  tx_debug_epConn := io.epConn
  tx_debug_debug1 := io.debug1

  // Debug
  val testReadAddr = RegInit(0x1000.U(64.W))
  when(epConn && io.debug1) {
    txQueue.io.enq.bits.addr := testReadAddr
    txQueue.io.enq.bits.chan := 1.U
    txQueue.io.enq.bits.opcode := 4.U
    txQueue.io.enq.bits.size := 6.U
    txQueue.io.enq.bits.data := 0.U
    txQueue.io.enq.valid := true.B

    testReadAddr := testReadAddr + 0x1000.U
  }

  val ackReadyFlag = RegInit(false.B)
  val ackTypeFlag = RegInit(0.U(2.W))
  val ackSeqNumFlag = RegInit(0.U(22.W))

  when(io.ackReady) {
    ackReadyFlag := io.ackReady
    ackTypeFlag := io.ackType
    ackSeqNumFlag := io.ackSeqNum
  }

  // Enqueue data into the queue when txValid is asserted
  // TODO txvalid is always high??
  when(io.tlValid) {
    txQueue.io.enq.bits.chan := io.tlChan
    txQueue.io.enq.bits.opcode := io.tlOpcode
    txQueue.io.enq.bits.param := io.tlParam
    txQueue.io.enq.bits.size := io.tlSize
    txQueue.io.enq.bits.source := io.tlSource
    txQueue.io.enq.bits.addr := io.tlAddr
    txQueue.io.enq.bits.data := io.tlData
    txQueue.io.enq.valid := true.B
  }

  val ackAckonly = RegInit(false.B)
  ackAckonly := io.ackAckonly
  
  val creditADecCntDebug = RegInit(0.U(22.W))
  val maxCreditChannelDebug = RegInit(0.U(3.W))
  val maxCreditDebug = RegInit(0.U(16.W))

  val txAccChannel = RegInit(0.U(3.W))
  val txAccCredit = RegInit(0.U(16.W))

  when(io.maxCreditChannel =/= 0.U) {
    maxCreditChannelDebug := io.maxCreditChannel
    maxCreditDebug := io.maxCredit  // Use the maxCredit output from FlowControl
  }

  val tx_debug_txState = RegInit(0.U(4.W))
  tx_debug_txState := txState

  val tx_debug_txQueueCnt = RegInit(0.U(3.W))
  tx_debug_txQueueCnt := txQueue.io.count

  def initNextQueueItem() = {
    nextChan := 0.U
    nextOpcode := 0.U
    nextParam := 0.U
    nextSize := 0.U
    nextSource := 0.U
    nextAddr := 0.U
    nextData := 0.U
  }

  switch(txState) {
    is(txIdle) {
      initNextQueueItem()

      txAccChannel := 0.U
      txAccCredit := 0.U

      when(txComplete) {
        when(ackAckonly) {
          txState := txAckOnly

        }.otherwise {
          txState := txCheckFrame
        }
      }
    }

    // TODO 처리할 메시지가 있으며 ackonly 프레임을 보내야할까?
    is(txAckOnly) {
      astate := amakeFrame
      txComplete := false.B

      txState := txDone
    }

    is(txCheckFrame) {
      //when (!io.retransmitIsFull && txQueue.io.deq.valid) {
      when (txQueue.io.deq.valid) {
        nextChan := txQueue.io.deq.bits.chan
        nextOpcode := txQueue.io.deq.bits.opcode
        nextParam := txQueue.io.deq.bits.param
        nextSize := txQueue.io.deq.bits.size
        nextSource := txQueue.io.deq.bits.source
        nextAddr := txQueue.io.deq.bits.addr
        nextData := txQueue.io.deq.bits.data
        txQueue.io.deq.ready := true.B

        isFrame := true.B
      }
      txState := txCheckAck
    }
    is(txCheckAck) {
      when(ackReadyFlag) {
        isACK := true.B
      }
      txState := txCheckCredit
    }


    // If AccChanCredits exists, transmit it together with the next packet
    is(txCheckCredit) {
      when(maxAccChannel =/= 0.U) {
        isCredit := true.B

        txAccChannel := maxAccChannel
        txAccCredit := maxAccCredit
      }
      txState := txInitFrame
    } 

    is(txInitFrame) {
      when(isFrame) {
        txFrame := OXPacket.initFrame(nextChan, nextAddr, nextOpcode, nextData, io.nextTxSeq, TLOESeqManager.getPrevSeq(io.nextRxSeq), io.ackType, txAccChannel, txAccCredit, nextSize, nextParam, nextSource)
        txFrameSize := nextSize

        //TODO false all??
        isFrame := false.B
        isACK := false.B
        txComplete := false.B

        ackReadyFlag := false.B
        ackTypeFlag := 0.U
        ackSeqNumFlag := 0.U

        txState := txHandleCredit
      }.elsewhen(isACK || isCredit) {
        txFrame := OXPacket.normalAck(io.nextTxSeq, TLOESeqManager.getPrevSeq(io.nextRxSeq), 1.U, txAccChannel, txAccCredit)
        txFrameSize := 0.U

        //TODO false all??
        isFrame := false.B
        isACK := false.B
        txComplete := false.B

        ackReadyFlag := false.B
        ackTypeFlag := 0.U
        ackSeqNumFlag := 0.U

        txState := txHandleAccCredit
      }.otherwise {
        txState := txDone
      }
    }

    is(txHandleCredit) {
      // Flow Control : decrease credit based on message type
      //val decFlits = TlMsgFlits.getFlitsCnt(nextChan, nextOpcode, nextSize)
      val decFlits = TloePacGen.getFlitSize(nextChan, nextOpcode, nextSize)

      when(io.credits(nextChan) >= decFlits) {
        io.decCreditValid   := true.B
        io.decCreditChannel := nextChan
        io.decCreditAmount  := decFlits 
        creditADecCntDebug  := creditADecCntDebug + decFlits
        txState := txHandleAccCredit
      }.otherwise {
        io.decCreditValid := false.B
        io.decCreditChannel := 0.U
        io.decCreditAmount := 0.U
        txState := txHandleCredit  // Stay in the same state if not enough credit
      }
    }

    is(txHandleAccCredit) {
      //Flow Control
      when (txAccChannel =/= 0.U) {
        io.decAccCreditChannel := txAccChannel
        io.decAccCreditAmount := (1.U << txAccCredit)
        io.decAccCreditValid := true.B

        isCredit := false.B
      }

      txState := txPrepareSend
    }

    is(txPrepareSend) {
      io.txData := txFrame
      io.txFlitSize := TloePacGen.getFlitSize(nextChan, nextOpcode, nextSize)
      io.txStart := true.B

      // Increase sequence number
      io.incTxSeq := true.B  // Set incTxSeq when incrementing TX sequence

      txState := txEnqRetransmit
    }

    is(txEnqRetransmit) {
        /*
      // Enqueue the packet to the retransmit buffer
      // TODO check if retransmitter is ready
      io.retransmitWrite.tloeFrame := txFrame
      //io.retransmitWrite.tloeFrameSize := txFrameSize
      io.retransmitWrite.tloeFrameSize := TloePacGen.getFlitSize(nextChan, nextOpcode, nextSize)
      io.retransmitWrite.state := 0.U(2.W)  // Initial state
      io.retransmitWrite.sendTime := io.currTime  // Use global timer 

      io.retransmitWriteValid := true.B  // Set valid signal when writing
      */

      txState := txDone
    }

    is(txDone) {
      when(txComplete) {
        txState := txIdle
        txComplete := true.B
      }
    }
  }

/*
  //Debug
  var isRetransmit = RegInit(false.B)
  isRetransmit := io.isRetransmit
*/

  //////////////////////////////////////////////////////////////////
  // Packet Sending Logic
  // TODO need to define as function
  val debug_txEtherTxReady = RegInit(false.B)
  debug_txEtherTxReady := io.txReady

  when(io.txReady) {
    sendPacket := false.B
    txComplete := true.B
  }

  //////////////////////////////////////////////////////////////////
  // SEND - Packet Send (Ack Only)

  val debug_nAckPacket = RegInit(0.U(512.W))
  debug_nAckPacket := nAckPacket(4223, 3712)

  // Send an ACKONLY frame
  switch(astate) {
    // Create normal frame
    is(amakeFrame) {
      nAckPacket := OXPacket.ackonly(io.nextTxSeq, io.nextRxSeq - 1.U, 1.U, 0.U, 0.U)
      astate := asendRequest
    }

    is(asendRequest) {
      io.txData := nAckPacket
      io.txFlitSize := 4.U
      io.txStart := true.B

      sendPacket := true.B // Indicate that packet is ready to send
      txComplete := false.B // Reset transmission complete flag
      io.ackAckonlyDone := true.B

      astate := adone // Move to wait f/isor credit acknowledgment
    }

    is(adone) {
      astate := aidle
    }
  }

  //////////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////////
  /*
  dontTouch(epConn)
  dontTouch(astate)
  dontTouch(tx_size)
  dontTouch(nextChan)
  dontTouch(nextOpcode)
  dontTouch(nextParam)
  dontTouch(nextSize)
  dontTouch(nextSource)
  dontTouch(nextAddr)
  dontTouch(nextData)
  dontTouch(isFrame)
  dontTouch(isACK)
  dontTouch(isCredit)
  dontTouch(maxAccChannel)
  dontTouch(maxAccCredit)

  dontTouch(txFrameSize)
  dontTouch(txRequiredFlits)

  dontTouch(sendPacket)
  dontTouch(txComplete)
  dontTouch(idx)

  dontTouch(axi_txdata)
  dontTouch(axi_txvalid)
  dontTouch(axi_txlast)
  dontTouch(axi_txkeep)

  dontTouch(testReadAddr)

  dontTouch(ackReadyFlag)
  dontTouch(ackTypeFlag)
  dontTouch(ackSeqNumFlag)

  dontTouch(debug_tlChan)
  dontTouch(debug_tlOpcode)
  dontTouch(debug_tlParam)
  dontTouch(debug_tlSize)
  dontTouch(debug_tlSource)
  dontTouch(debug_tlAddr)
  dontTouch(debug_tlData)
  dontTouch(debug_tlValid)

  dontTouch(creditADecCntDebug)
  dontTouch(maxCreditChannelDebug)
  dontTouch(maxCreditDebug)
  dontTouch(txAccChannel)
  dontTouch(txAccCredit)

  dontTouch(isRetransmit)
  dontTouch(debug_txAckAckonly)
  dontTouch(debug_txEtherTxReady)
  dontTouch(ackAckonly)
  dontTouch(debug_nAckPacket)
  dontTouch(debug_txAckCnt)
  dontTouch(debug_tloeEtherTxData)
  dontTouch(debug_tloeEtherTxFlitSize)
  dontTouch(txState)

  dontTouch(debug_txFrame)
  dontTouch(debug_txFrameSize)
  */
} 