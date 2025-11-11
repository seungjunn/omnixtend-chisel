package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

/**
 * TLOEEndpoint Module
 * 
 * This is the top-level module that integrates all TLOE components:
 * - TLOETransmitter: Handles packet transmission
 * - TLOEReceiver: Handles packet reception
 * - TLOESeqManager: Manages sequence numbers
 * - FlowControl: Manages credits
 * - GlobalTimer: Provides global time
 * - Retransmission: Handles packet retransmission
 * - TLOEEtherQSFP1: Ethernet interface
 * 
 * This module acts as the main coordinator for all TLOE operations.
 */
class TLOEEndpoint extends Module {
    val io = IO(new Bundle {
    // ========================================================================
    // TileLink Interface - Input from TileLink protocol
    // ========================================================================
    // TileLink Interface
    val tlChan = Input(UInt(3.W))      // TileLink channel
    val tlOpcode = Input(UInt(3.W))    // TileLink operation code
    val tlParam = Input(UInt(4.W))     // TileLink parameter
    val tlSize = Input(UInt(4.W))      // Transfer size
    val tlSource = Input(UInt(26.W))   // Source ID
    val tlAddr = Input(UInt(64.W))     // Address
    val tlData = Input(UInt(512.W))    // Data payload
    val tlMask = Input(UInt(64.W))     // Byte enable mask
    val tlValid = Input(Bool())        // Valid signal

    val modeConn = Output(UInt(2.W))   // Connection mode output

    // ========================================================================
    // Ethernet Interface - External connection
    // ========================================================================
    // Ethernet Interface
    val txdata = Output(UInt(TLOE_PACKET_SIZE.W))  // TX data (880 bits)
    val txvalid = Output(Bool())                    // TX valid
    val txlast = Output(Bool())                     // TX last
    val txkeep = Output(UInt(8.W))                  // TX keep mask

    val txready = Input(Bool())        // TX ready from Ethernet
    val rxdata = Input(UInt(512.W))    // RX data from Ethernet
    val rxvalid = Input(Bool())        // RX valid
    val rxlast = Input(Bool())         // RX last

    // ========================================================================
    // VIO (Virtual I/O) Control Signals
    // ========================================================================
    // VIO
    val ox_open = Input(Bool())         // Open connection
    val ox_close = Input(Bool())        // Close connection
    val ox_debug1 = Input(UInt(64.W))   // Debug signal 1
    val ox_debug2 = Input(UInt(64.W))   // Debug signal 2

    // ========================================================================
    // TileLink Handler Interface
    // ========================================================================
    // TileLink Handler signals
    val tlMsg = Output(UInt(4096.W))           // Extracted TileLink message
    val tlMsgMask = Output(UInt(64.W))         // Message mask
    val doTilelinkHandler = Output(Bool())     // Trigger TileLink handler

    // ========================================================================
    // Flow Control Interface
    // ========================================================================
    val incAccCreditValid = Input(Bool())      // Increment accumulated credit valid
    val incAccCreditChannel = Input(UInt(3.W)) // Channel for accumulated credit
    val incAccCreditAmount = Input(UInt(5.W))  // Amount of accumulated credit

    // ========================================================================
    // MAC Address Configuration
    // ========================================================================
    val setSrcMac = Input(UInt(48.W))  // Set source MAC address
    val setDestMac = Input(UInt(48.W)) // Set destination MAC address

    val srcMac = Output(UInt(48.W))   // Current source MAC address
    val destMac = Output(UInt(48.W))  // Current destination MAC address
  })

  // ========================================================================
  // Internal State Registers
  // ========================================================================
  val connMode = RegInit(MODE_MASTER)
  connMode := io.modeConn

  val isRetransmit = RegInit(false.B)  // Retransmission active flag
  val isConn = RegInit(false.B)         // Connection established flag

  // ========================================================================
  // RX Data Buffering Registers
  // ========================================================================
  val rxdata_ep = RegInit(0.U(64.W))
  val rxvalid_ep = RegInit(false.B)
  val rxlast_ep = RegInit(false.B)
  rxdata_ep := io.rxdata
  rxvalid_ep := io.rxvalid
  rxlast_ep := io.rxlast

  // ========================================================================
  // Module Instantiations
  // ========================================================================
  val transmitter    = Module(new TLOETransmitter)    // Packet transmitter
  val receiver       = Module(new TLOEReceiver)       // Packet receiver
  val tloeSeqNum     = Module(new TLOESeqManager)    // Sequence number manager
  val flowControl    = Module(new FlowControl)        // Flow control manager
  val timer          = Module(new GlobalTimer)        // Global timer
  val retransmission = Module(new Retransmission)     // Retransmission handler
  val ether_qsfp1    = Module(new TLOEEtherQSFP1)     // Ethernet interface

  // ========================================================================
  // Default Initialization
  // ========================================================================
  // Initialize TLOEEther internal interface
  ether_qsfp1.io.internalTxData := 0.U
  ether_qsfp1.io.internalTxValid := false.B

  io.srcMac := 0.U
  io.destMac := 0.U

  // ========================================================================
  // TileLink Handler Connection
  // ========================================================================
  val doth_ep = RegInit(false.B)
  doth_ep := receiver.io.doTilelinkHandler

  // Connect to TileLink_Handler
  io.tlMsg := receiver.io.tlMsg
  io.tlMsgMask := receiver.io.tlMsgMask
  io.doTilelinkHandler := receiver.io.doTilelinkHandler

  // ========================================================================
  // Ethernet Interface Connections
  // ========================================================================
  // QSFP1
  // Connect TLOEEther to external interface (TX)
  io.txdata := ether_qsfp1.io.txdata
  io.txvalid := ether_qsfp1.io.txvalid
  io.txlast := ether_qsfp1.io.txlast
  io.txkeep := ether_qsfp1.io.txkeep
  ether_qsfp1.io.txready := io.txready
  
  // Connect external ethernet signals to TLOEEther (RX)
  ether_qsfp1.io.rxdata := io.rxdata
  ether_qsfp1.io.rxvalid := io.rxvalid
  ether_qsfp1.io.rxlast := io.rxlast

  // ========================================================================
  // Internal TX Interface - Priority Multiplexing
  // ========================================================================
  // Connect endpoint ready signal to TLOEEther
  ether_qsfp1.io.endpointRxReady := receiver.io.rxReady
  
  transmitter.io.txReady := ether_qsfp1.io.internalTxReady
  retransmission.io.txReady := ether_qsfp1.io.internalTxReady
  
  // Connect modules to TLOEEther internal interface
  // Priority: transmitter > retransmission
  when(transmitter.io.txStart) {
    // Transmitter module first priority
    ether_qsfp1.io.internalTxData := transmitter.io.txData
    ether_qsfp1.io.internalTxFlitSize := transmitter.io.txFlitSize
    ether_qsfp1.io.internalTxValid := true.B  // Signal to add packet to queue
  }.elsewhen(retransmission.io.txStart) {
    // Retransmission module second priority
    ether_qsfp1.io.internalTxData := retransmission.io.txData
    ether_qsfp1.io.internalTxFlitSize := retransmission.io.txFlitSize
    ether_qsfp1.io.internalTxValid := true.B  // Signal to add packet to queue
  }.otherwise {
    // No module requesting TX
    ether_qsfp1.io.internalTxData := 0.U
    ether_qsfp1.io.internalTxValid := false.B
    ether_qsfp1.io.internalTxFlitSize := 0.U
  }

  // ========================================================================
  // Sequence Manager Connections
  // ========================================================================
  // Sequence Manager
  tloeSeqNum.io.reset := false.B  // Reset is handled by the node's reset
  tloeSeqNum.io.incTxSeq := transmitter.io.incTxSeq
  tloeSeqNum.io.incRxSeq := receiver.io.incRxSeq
  tloeSeqNum.io.updateAckSeq := receiver.io.updateAckSeq
  tloeSeqNum.io.newAckSeq := receiver.io.newAckSeq

  // Connect sequence numbers to transmitter
  transmitter.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  transmitter.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  transmitter.io.ackdSeq := tloeSeqNum.io.ackdSeq

  // Connect sequence numbers to receiver
  receiver.io.nextTxSeq := tloeSeqNum.io.nextTxSeq
  receiver.io.nextRxSeq := tloeSeqNum.io.nextRxSeq

  // ========================================================================
  // TileLink Interface to Transmitter
  // ========================================================================
  transmitter.io.tlChan := io.tlChan
  transmitter.io.tlOpcode := io.tlOpcode
  transmitter.io.tlParam := io.tlParam
  transmitter.io.tlSize := io.tlSize
  transmitter.io.tlSource := io.tlSource
  transmitter.io.tlAddr := io.tlAddr
  transmitter.io.tlData := io.tlData
  transmitter.io.tlMask := io.tlMask
  transmitter.io.tlValid := io.tlValid

  // ========================================================================
  // RX Routing from Ethernet to Receiver
  // ========================================================================
  // Route ethernet RX from TLOEEther to appropriate modules based on isConn state and ready status
  when(ether_qsfp1.io.endpointRxValid) {
      // Normal mode: send to receiver module
      when(receiver.io.rxReady) {
        receiver.io.rxFrame := ether_qsfp1.io.endpointRxData
        receiver.io.rxValid := ether_qsfp1.io.endpointRxValid
        receiver.io.rxFlitSize := ether_qsfp1.io.endpointRxFlitSize
      }.otherwise {
        // Receiver not ready - hold data
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

  // ========================================================================
  // Receiver Connections
  // ========================================================================
  receiver.io.nextRxSeq := tloeSeqNum.io.nextRxSeq
  receiver.io.slideDone := retransmission.io.slideDone
  receiver.io.retransmitDone := retransmission.io.retransmitDone

  // Sequence Management
  tloeSeqNum.io.reset := false.B

  // ========================================================================
  // Flow Control Connections
  // ========================================================================
  // Flow Control
  // TODO: MUX??? (consider if needed)
  // Transmitter decrements credits
  flowControl.io.decCredit.valid := transmitter.io.decCreditValid
  flowControl.io.decCredit.channel := transmitter.io.decCreditChannel
  flowControl.io.decCredit.credit := transmitter.io.decCreditAmount
  flowControl.io.decAccCredit.valid := transmitter.io.decAccCreditValid
  flowControl.io.decAccCredit.channel := transmitter.io.decAccCreditChannel
  flowControl.io.decAccCredit.credit := transmitter.io.decAccCreditAmount

  // Transmitter reads credit status
  // TODO: 사용하는지??? credits, accCredits, error
  transmitter.io.credits := flowControl.io.credits
  transmitter.io.maxCreditChannel := flowControl.io.maxCreditChannel
  transmitter.io.maxCredit := flowControl.io.maxCredit

  // Receiver increments credits
  flowControl.io.incCredit.valid := receiver.io.incCreditValid
  flowControl.io.incCredit.channel := receiver.io.incCreditChannel
  flowControl.io.incCredit.credit := receiver.io.incCreditAmount

  flowControl.io.incAccCredit.valid := receiver.io.incAccCreditValid
  flowControl.io.incAccCredit.channel := receiver.io.incAccCreditChannel
  flowControl.io.incAccCredit.credit := receiver.io.incAccCreditAmount
  
  // TODO: 사용하는지??? credits, accCredits, error
  //receiver.io.credits := flowControl.io.credits
  //receiver.io.accCredits := flowControl.io.accCredits
  //receiver.io.error := flowControl.io.error

  // ========================================================================
  // Global Timer Connections
  // ========================================================================
  // Global Timer
  timer.io.resetTimer := false.B
  transmitter.io.currTime := timer.io.globalTimer
  transmitter.io.modeConn := io.modeConn
  retransmission.io.currTime := timer.io.globalTimer

  // ========================================================================
  // Retransmission Connections
  // ========================================================================
  // Retransmission
  retransmission.io.clear := false.B
  retransmission.io.retransmitSeqNum := receiver.io.retransmitSeqNum
  retransmission.io.retransmitValid := receiver.io.retransmitValid
  retransmission.io.slideSeqNumAck := receiver.io.slideSeqNumAck
  retransmission.io.slideValid := receiver.io.slideValid

  retransmission.io.read := 0.U(896.W)  // Unused read interface
  retransmission.io.write := transmitter.io.retransmitWrite
  retransmission.io.writeValid := transmitter.io.retransmitWriteValid
  transmitter.io.retransmitIsFull := retransmission.io.isFull
  transmitter.io.isRetransmit := retransmission.io.isRetransmit

  isRetransmit := retransmission.io.isRetransmit

  // Connect connection module to TLOEEther (already done above)
  // connection.io.tloeEtherTxData, tloeEtherTxStart, tloeEtherTxReady

  // ========================================================================
  // Connection Status
  // ========================================================================
  isConn := true.B 
  transmitter.io.epConn := true.B

  io.modeConn := MODE_MASTER
  transmitter.io.modeConn := io.modeConn
  ether_qsfp1.io.modeConn := io.modeConn

  // ========================================================================
  // Transmitter <-> Receiver ACK Interface
  // ========================================================================
  // transmitter <> receiver
  transmitter.io.ackSeqNum := receiver.io.ackSeqNum
  transmitter.io.ackType := receiver.io.ackType
  transmitter.io.ackReady := receiver.io.ackReady
  transmitter.io.ackAckonly := receiver.io.ackAckonly
  receiver.io.ackAckonlyDone := transmitter.io.ackAckonlyDone

  // ========================================================================
  // Debug Interface
  // ========================================================================
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
