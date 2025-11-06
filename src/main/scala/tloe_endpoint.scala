package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

class TLOEEndpoint extends Module {
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

    val modeConn = Output(UInt(2.W))

    // Ethernet Interface
    val txdata = Output(UInt(TLOE_PACKET_SIZE.W))  // 64-bit for actual ethernet transmission
    val txvalid = Output(Bool())
    val txlast = Output(Bool())
    val txkeep = Output(UInt(8.W))

    val txready = Input(Bool())
    val rxdata = Input(UInt(512.W))
    val rxvalid = Input(Bool())
    val rxlast = Input(Bool())

    // VIO
    val ox_open = Input(Bool())
    val ox_close = Input(Bool())
    val ox_debug1 = Input(UInt(64.W))
    val ox_debug2 = Input(UInt(64.W))

    // TileLink Handler signals
    val tlMsg = Output(UInt(4096.W))
    val tlMsgMask = Output(UInt(64.W))
    val doTilelinkHandler = Output(Bool())

    val incAccCreditValid = Input(Bool())
    val incAccCreditChannel = Input(UInt(3.W))
    val incAccCreditAmount = Input(UInt(5.W))

    val setSrcMac = Input(UInt(48.W))
    val setDestMac = Input(UInt(48.W))

    val srcMac = Output(UInt(48.W))
    val destMac = Output(UInt(48.W))
  })

  val connMode = RegInit(MODE_MASTER)
  connMode := io.modeConn

  val isRetransmit = RegInit(false.B)
  val isConn = RegInit(false.B)

  val rxdata_ep = RegInit(0.U(64.W))
  val rxvalid_ep = RegInit(false.B)
  val rxlast_ep = RegInit(false.B)
  rxdata_ep := io.rxdata
  rxvalid_ep := io.rxvalid
  rxlast_ep := io.rxlast

  val transmitter    = Module(new TLOETransmitter)
  val receiver       = Module(new TLOEReceiver)
  val tloeSeqNum     = Module(new TLOESeqManager)
  val flowControl    = Module(new FlowControl)
  val timer          = Module(new GlobalTimer)
  val retransmission = Module(new Retransmission)
  val ether_qsfp1    = Module(new TLOEEtherQSFP1)

  // Initialize TLOEEther internal interface
  ether_qsfp1.io.internalTxData := 0.U
  ether_qsfp1.io.internalTxValid := false.B

  io.srcMac := 0.U
  io.destMac := 0.U

  val doth_ep = RegInit(false.B)
  doth_ep := receiver.io.doTilelinkHandler

  // Connect to TileLink_Handler
  io.tlMsg := receiver.io.tlMsg
  io.tlMsgMask := receiver.io.tlMsgMask
  io.doTilelinkHandler := receiver.io.doTilelinkHandler

  // QSFP1
  // Connect TLOEEther to external interface
  io.txdata := ether_qsfp1.io.txdata
  io.txvalid := ether_qsfp1.io.txvalid
  io.txlast := ether_qsfp1.io.txlast
  io.txkeep := ether_qsfp1.io.txkeep
  ether_qsfp1.io.txready := io.txready
  
  // Connect external ethernet signals to TLOEEther
  ether_qsfp1.io.rxdata := io.rxdata
  ether_qsfp1.io.rxvalid := io.rxvalid
  ether_qsfp1.io.rxlast := io.rxlast

  // Connect endpoint ready signal to TLOEEther
  ether_qsfp1.io.endpointRxReady := receiver.io.rxReady
  
  transmitter.io.txReady := ether_qsfp1.io.internalTxReady
  //retransmission.io.txReady := ether_qsfp1.io.internalTxReady
  
  // Connect modules to TLOEEther internal interface
  // Priority: connection > transmitter > retransmission
  when(transmitter.io.txStart) {
    // Transmitter module second priority
    ether_qsfp1.io.internalTxData := transmitter.io.txData
    ether_qsfp1.io.internalTxFlitSize := transmitter.io.txFlitSize
    ether_qsfp1.io.internalTxValid := true.B  // Signal to add packet to queue
    /*
  }.elsewhen(retransmission.io.txStart) {
    // Retransmission module lowest priority
    ether_qsfp1.io.internalTxData := retransmission.io.txData
    ether_qsfp1.io.internalTxFlitSize := retransmission.io.txFlitSize
    ether_qsfp1.io.internalTxValid := true.B  // Signal to add packet to queue
    */
  }.otherwise {
    // No module requesting TX
    ether_qsfp1.io.internalTxData := 0.U
    ether_qsfp1.io.internalTxValid := false.B
    ether_qsfp1.io.internalTxFlitSize := 0.U
  }

  // Sequence Manager
  tloeSeqNum.io.reset := false.B  // Reset is handled by the node's reset
  tloeSeqNum.io.incTxSeq := transmitter.io.incTxSeq
  tloeSeqNum.io.incRxSeq := receiver.io.incRxSeq
  tloeSeqNum.io.updateAckSeq := receiver.io.updateAckSeq
  tloeSeqNum.io.newAckSeq := receiver.io.newAckSeq

  transmitter.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  transmitter.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  transmitter.io.ackdSeq := tloeSeqNum.io.ackdSeq

  receiver.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  receiver.io.nextRxSeq := tloeSeqNum.io.nextRxSeq

  transmitter.io.tlChan := io.tlChan
  transmitter.io.tlOpcode := io.tlOpcode
  transmitter.io.tlParam := io.tlParam
  transmitter.io.tlSize := io.tlSize
  transmitter.io.tlSource := io.tlSource
  transmitter.io.tlAddr := io.tlAddr
  transmitter.io.tlData := io.tlData
  transmitter.io.tlMask := io.tlMask
  transmitter.io.tlValid := io.tlValid

  // Route ethernet RX from TLOEEther to appropriate modules based on isConn state and ready status
  when(ether_qsfp1.io.endpointRxValid) {
      // Normal mode: send to receiver module
      when(receiver.io.rxReady) {
        receiver.io.rxFrame := ether_qsfp1.io.endpointRxData
        receiver.io.rxValid := ether_qsfp1.io.endpointRxValid
        receiver.io.rxFlitSize := ether_qsfp1.io.endpointRxFlitSize
      }.otherwise {
        receiver.io.rxFrame := 0.U
        receiver.io.rxValid := false.B
        receiver.io.rxFlitSize := 0.U
      }
  }.otherwise {
    // No valid RX data from ether_qsfp1
    receiver.io.rxFrame := 0.U
    receiver.io.rxValid := false.B
    receiver.io.rxFlitSize := 0.U
  }

  receiver.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
/*
  receiver.io.slideDone := retransmission.io.slideDone
  receiver.io.retransmitDone := retransmission.io.retransmitDone
  */

  // Sequence Management
  tloeSeqNum.io.reset := false.B

  // Flow Control
  // TODO MUX???
  flowControl.io.decCredit.valid := transmitter.io.decCreditValid
  flowControl.io.decCredit.channel := transmitter.io.decCreditChannel
  flowControl.io.decCredit.credit := transmitter.io.decCreditAmount
  flowControl.io.decAccCredit.valid := transmitter.io.decAccCreditValid
  flowControl.io.decAccCredit.channel := transmitter.io.decAccCreditChannel
  flowControl.io.decAccCredit.credit := transmitter.io.decAccCreditAmount

  // TODO 사용하는지??? credits, accCredits, error
  transmitter.io.credits := flowControl.io.credits
  transmitter.io.maxCreditChannel := flowControl.io.maxCreditChannel
  transmitter.io.maxCredit := flowControl.io.maxCredit

  // Flow Control
  flowControl.io.incCredit.valid := receiver.io.incCreditValid
  flowControl.io.incCredit.channel := receiver.io.incCreditChannel
  flowControl.io.incCredit.credit := receiver.io.incCreditAmount

  flowControl.io.incAccCredit.valid := receiver.io.incAccCreditValid
  flowControl.io.incAccCredit.channel := receiver.io.incAccCreditChannel
  flowControl.io.incAccCredit.credit := receiver.io.incAccCreditAmount
  
  // TODO 사용하는지??? credits, accCredits, error
  //receiver.io.credits := flowControl.io.credits
  //receiver.io.accCredits := flowControl.io.accCredits
  //receiver.io.error := flowControl.io.error

  // Global Timer
  timer.io.resetTimer := false.B
  transmitter.io.currTime := timer.io.globalTimer
  transmitter.io.modeConn := io.modeConn
  /*
  retransmission.io.currTime := timer.io.globalTimer

  // Retransmission
  retransmission.io.clear := false.B
  retransmission.io.retransmitSeqNum := receiver.io.retransmitSeqNum
  retransmission.io.retransmitValid := receiver.io.retransmitValid
  retransmission.io.slideSeqNumAck := receiver.io.slideSeqNumAck
  retransmission.io.slideValid := receiver.io.slideValid

  retransmission.io.read := 0.U(896.W)
  retransmission.io.write := transmitter.io.retransmitWrite
  retransmission.io.writeValid := transmitter.io.retransmitWriteValid
  transmitter.io.retransmitIsFull := retransmission.io.isFull
  transmitter.io.isRetransmit := retransmission.io.isRetransmit

  isRetransmit := retransmission.io.isRetransmit
  */

  // Connect connection module to TLOEEther (already done above)
  // connection.io.tloeEtherTxData, tloeEtherTxStart, tloeEtherTxReady

  isConn := true.B 
  transmitter.io.epConn := true.B

  io.modeConn := MODE_MASTER
  transmitter.io.modeConn := io.modeConn
  ether_qsfp1.io.modeConn := io.modeConn

  // transmitter <> receiver
  transmitter.io.ackSeqNum := receiver.io.ackSeqNum
  transmitter.io.ackType := receiver.io.ackType
  transmitter.io.ackReady := receiver.io.ackReady
  transmitter.io.ackAckonly := receiver.io.ackAckonly
  receiver.io.ackAckonlyDone := transmitter.io.ackAckonlyDone

  transmitter.io.debug1 := io.ox_debug1
  transmitter.io.debug2 := io.ox_debug2

  //////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////
  /*
  dontTouch(doth_ep)
  dontTouch(connMode)
  dontTouch(isRetransmit)
  dontTouch(isConn)
  dontTouch(rxdata_ep)
  dontTouch(rxvalid_ep)
  dontTouch(rxlast_ep)
  */
} 
