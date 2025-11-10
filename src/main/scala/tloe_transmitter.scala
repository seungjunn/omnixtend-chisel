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

  // Reduced from 12 to 8 states to save LUTs
  // Merged: txCheckAck+txCheckCredit -> txCheckFrame
  // Merged: txHandleAccCredit+txPrepareSend+txEnqRetransmit -> txHandleCredit
  val txIdle :: txAckOnly :: txCheckFrame :: txInitFrame :: txHandleCredit :: txSendPacket :: txDone :: Nil = Enum(7)
  val txState = RegInit(txIdle)

  val aidle :: amakeFrame :: asendRequest :: adone :: Nil = Enum(4)
  val astate = RegInit(aidle)

  // val tx_size = RegInit(0.U(3.W))  // Removed - unused, saves LUTs

  val nextChan = RegInit(0.U(3.W))
  val nextOpcode = RegInit(0.U(3.W))
  val nextParam = RegInit(0.U(4.W))
  val nextSize = RegInit(0.U(4.W))
  val nextSource = RegInit(0.U(26.W))
  val nextAddr = RegInit(0.U(64.W))
  // Removed nextData (512 bits) to save massive LUTs - use directly from queue
  // val nextData = RegInit(0.U(512.W))

  val isFrame = RegInit(false.B)
  val isACK = RegInit(false.B)
  val isCredit = RegInit(false.B)

  val maxAccChannel = RegInit(0.U(3.W))
  val maxAccCredit = RegInit(0.U(16.W))
  maxAccChannel := io.maxCreditChannel
  maxAccCredit := io.maxCredit

  // MASSIVE LUT REDUCTION: Remove frame buffers completely!
  // txFrameVec (4224 bits) and nAckPacketVec (4224 bits) REMOVED
  // Generate packets directly to output instead of buffering
  // This saves ~8500 LUTs from registers + Cat() operations
  
  val txFrameSize = RegInit(0.U(5.W))
  val txRequiredFlits = RegInit(0.U(8.W))
  
  // Wire for direct packet generation (no buffering)
  val txPacketWire = Wire(UInt(TLOE_FRAME_SIZE.W))
  txPacketWire := 0.U  // Default value

  // val sendPacket = RegInit(false.B)  // Removed - unused
  val txComplete = RegInit(true.B)
  // val idx = RegInit(0.U(16.W))  // Removed - unused

  // AXI-Stream registers removed to save LUTs (not used in current design)
  // val axi_txdata = RegInit(0.U(64.W))
  // val axi_txvalid = RegInit(false.B)
  // val axi_txlast = RegInit(false.B)
  // val axi_txkeep = RegInit(0.U(8.W))  

  // Prepare data for TLOEEther's internal interface
  // TLOEEther will handle the conversion to ethernet signals
  // Note: TLOEEther is instantiated in OX.scala

   // Single integrated queue for transmission data
  // Reduced depth from 16 to 4 to save LUTs
  val txQueue = Module(new Queue(new Bundle {
    val chan = UInt(3.W)
    val opcode = UInt(3.W)
    val param = UInt(4.W)
    val size = UInt(4.W)
    val source = UInt(26.W)
    val addr = UInt(64.W)
    val data = UInt(512.W)
  }, 4))

  // Default queue port values
  txQueue.io.enq.valid := false.B
  txQueue.io.enq.bits := 0.U.asTypeOf(txQueue.io.enq.bits)
  txQueue.io.deq.ready := false.B

  // Debug registers removed to save LUTs
  // val tx_debug_epConn = RegInit(false.B)
  // val tx_debug_debug1 = RegInit(false.B)
  // tx_debug_epConn := io.epConn
  // tx_debug_debug1 := io.debug1

  // Debug - test read address generation
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

  // Simplified: use direct IO instead of intermediate flags to save LUTs
  val ackReadyFlag = RegInit(false.B)
  // val ackTypeFlag = RegInit(0.U(2.W))  // Removed - use io.ackType directly
  // val ackSeqNumFlag = RegInit(0.U(22.W))  // Removed - use io.ackSeqNum directly

  when(io.ackReady) {
    ackReadyFlag := io.ackReady
    // Use io.ackType and io.ackSeqNum directly instead of buffering
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
  
  // Debug registers removed to save LUTs
  // val creditADecCntDebug = RegInit(0.U(22.W))
  // val maxCreditChannelDebug = RegInit(0.U(3.W))
  // val maxCreditDebug = RegInit(0.U(16.W))

  val txAccChannel = RegInit(0.U(3.W))
  val txAccCredit = RegInit(0.U(16.W))

  // Removed debug signal assignment to save LUTs
  // when(io.maxCreditChannel =/= 0.U) {
  //   maxCreditChannelDebug := io.maxCreditChannel
  //   maxCreditDebug := io.maxCredit
  // }

  // val tx_debug_txState = RegInit(0.U(4.W))
  // tx_debug_txState := txState

  // val tx_debug_txQueueCnt = RegInit(0.U(3.W))
  // tx_debug_txQueueCnt := txQueue.io.count

  def initNextQueueItem() = {
    nextChan := 0.U
    nextOpcode := 0.U
    nextParam := 0.U
    nextSize := 0.U
    nextSource := 0.U
    nextAddr := 0.U
    // nextData removed to save LUTs
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
      // Merged txCheckAck and txCheckCredit to save states
      when (txQueue.io.deq.valid) {
        nextChan := txQueue.io.deq.bits.chan
        nextOpcode := txQueue.io.deq.bits.opcode
        nextParam := txQueue.io.deq.bits.param
        nextSize := txQueue.io.deq.bits.size
        nextSource := txQueue.io.deq.bits.source
        nextAddr := txQueue.io.deq.bits.addr
        // Don't copy data - use directly from queue to save 512 bits of registers
        txQueue.io.deq.ready := true.B
        isFrame := true.B
      }
      
      // Check ACK in same state
      when(ackReadyFlag) {
        isACK := true.B
      }
      
      // Check credit in same state
      when(maxAccChannel =/= 0.U) {
        isCredit := true.B
        txAccChannel := maxAccChannel
        txAccCredit := maxAccCredit
      }
      
      txState := txInitFrame
    } 

    is(txInitFrame) {
      when(isFrame) {
        // Generate packet directly - no buffering!
        txPacketWire := OXPacket.initFrame(nextChan, nextAddr, nextOpcode, 0.U, io.nextTxSeq, TLOESeqManager.getPrevSeq(io.nextRxSeq), io.ackType, txAccChannel, txAccCredit, nextSize, nextParam, nextSource)
        txFrameSize := nextSize
        isFrame := false.B
        isACK := false.B
        ackReadyFlag := false.B
        txState := txHandleCredit
      }.elsewhen(isACK || isCredit) {
        // Generate ACK packet directly - no buffering!
        txPacketWire := OXPacket.normalAck(io.nextTxSeq, TLOESeqManager.getPrevSeq(io.nextRxSeq), 1.U, txAccChannel, txAccCredit)
        txFrameSize := 0.U
        isFrame := false.B
        isACK := false.B
        ackReadyFlag := false.B
        txState := txSendPacket  // Skip credit handling for ACK
      }.otherwise {
        txPacketWire := 0.U
        txState := txDone
      }
    }

    is(txHandleCredit) {
      // Merged txHandleAccCredit, txPrepareSend, txEnqRetransmit to save states
      val decFlits = TloePacGen.getFlitSize(nextChan, nextOpcode, nextSize)

      when(io.credits(nextChan) >= decFlits) {
        io.decCreditValid   := true.B
        io.decCreditChannel := nextChan
        io.decCreditAmount  := decFlits
        
        // Handle accumulated credit in same state
        when (txAccChannel =/= 0.U) {
          io.decAccCreditChannel := txAccChannel
          io.decAccCreditAmount := (1.U << txAccCredit)
          io.decAccCreditValid := true.B
          isCredit := false.B
        }
        
        txState := txSendPacket
      }.otherwise {
        io.decCreditValid := false.B
        txState := txHandleCredit  // Wait for credit
      }
    }

    is(txSendPacket) {
      // Send directly from wire - no Cat() operation needed!
      io.txData := txPacketWire
      io.txFlitSize := TloePacGen.getFlitSize(nextChan, nextOpcode, nextSize)
      io.txStart := true.B
      io.incTxSeq := true.B
      txComplete := false.B
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
  // Debug register removed to save LUTs
  // val debug_txEtherTxReady = RegInit(false.B)
  // debug_txEtherTxReady := io.txReady

  when(io.txReady) {
    // sendPacket := false.B  // Removed
    txComplete := true.B
  }

  //////////////////////////////////////////////////////////////////
  // SEND - Packet Send (Ack Only)

  // Debug register removed to save LUTs
  // val debug_nAckPacket = RegInit(0.U(512.W))
  // debug_nAckPacket := nAckPacketAsUInt(4223, 3712)

  // Wire for ACK-only packet (no buffering)
  val ackOnlyPacketWire = Wire(UInt(TLOE_FRAME_SIZE.W))
  ackOnlyPacketWire := 0.U

  // Send an ACKONLY frame - direct generation, no buffering!
  switch(astate) {
    is(amakeFrame) {
      // Generate directly instead of buffering
      ackOnlyPacketWire := OXPacket.ackonly(io.nextTxSeq, io.nextRxSeq - 1.U, 1.U, 0.U, 0.U)
      astate := asendRequest
    }

    is(asendRequest) {
      // Send directly - no Cat() operation!
      io.txData := ackOnlyPacketWire
      io.txFlitSize := 4.U
      io.txStart := true.B
      txComplete := false.B
      io.ackAckonlyDone := true.B
      astate := adone
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