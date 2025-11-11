package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._
import TloePacGen._

/**
 * TLOETransmitter Module
 * 
 * This module handles the transmission of TLOE (TileLink over Ethernet) packets.
 * It manages packet generation, flow control, sequence numbers, and retransmission.
 * 
 * Main responsibilities:
 * - Receive TileLink requests and convert them to TLOE packets
 * - Handle ACK-only packets
 * - Manage flow control credits
 * - Generate sequence numbers
 * - Enqueue packets to retransmission buffer
 */
class TLOETransmitter extends Module {
  val io = IO(new Bundle {
    // ========================================================================
    // TileLink Interface - Input from TileLink protocol
    // ========================================================================
    val tlChan = Input(UInt(3.W))      // Channel: 0=Acquire, 1=Release, 2=Grant, 3=Probe, 4=AccessAck, 5=AccessAckData
    val tlOpcode = Input(UInt(3.W))    // Operation code (GET, PUT, etc.)
    val tlParam = Input(UInt(4.W))     // Parameter field
    val tlSize = Input(UInt(4.W))      // Transfer size (log2 of bytes)
    val tlSource = Input(UInt(26.W))   // Source ID for transaction
    val tlAddr = Input(UInt(64.W))     // Address for the transaction
    val tlData = Input(UInt(512.W))    // Data payload (512 bits)
    val tlMask = Input(UInt(64.W))     // Byte enable mask
    val tlValid = Input(Bool())        // Valid signal for TileLink transaction

    // ========================================================================
    // TLOEEther Interface - Output to Ethernet layer
    // ========================================================================
    val txData = Output(UInt(TLOE_FRAME_SIZE.W))  // TLOE frame data to transmit
    val txFlitSize = Output(UInt(7.W))            // Number of flits in the frame
    val txStart = Output(Bool())                   // Start transmission signal
    val txReady = Input(Bool())                    // Ready signal from Ethernet layer

    // ========================================================================
    // Sequence Management - Control sequence numbers for reliable delivery
    // ========================================================================
    val incTxSeq = Output(Bool())        // Increment TX sequence number
    val incRxSeq = Output(Bool())        // Increment RX sequence number
    val updateAckSeq = Output(Bool())    // Update acknowledged sequence number
    val newAckSeq = Output(UInt(22.W))   // New ACK sequence number to set

    val nextTxSeq = Input(UInt(22.W))    // Current TX sequence number from seq manager
    val nextRxSeq = Input(UInt(22.W))    // Current RX sequence number from seq manager
    val ackdSeq = Input(UInt(22.W))      // Last acknowledged sequence number

    // ========================================================================
    // Flow Control - Credit-based flow control for channels
    // ========================================================================
    val decCreditValid = Output(Bool())        // Decrement credit valid signal
    val decCreditChannel = Output(UInt(3.W))   // Channel to decrement credit
    val decCreditAmount = Output(UInt(16.W))   // Amount of credit to decrement

    val decAccCreditValid = Output(Bool())    // Decrement accumulated credit valid
    val decAccCreditChannel = Output(UInt(3.W)) // Channel for accumulated credit
    val decAccCreditAmount = Output(UInt(16.W)) // Amount of accumulated credit to decrement

    val credits = Input(Vec(6, UInt(16.W)))   // Current credits for each channel
    //val accCredits = Input(Vec(6, UInt(16.W)))  // Accumulated credits (unused)
    val maxCreditChannel = Input(UInt(3.W))   // Channel with maximum credit to send
    //val error = Input(Bool())                 // Error signal (unused)
    val maxCredit = Input(UInt(16.W))         // Maximum credit value to send

    // ========================================================================
    // Retransmission - Buffer for reliable packet delivery
    // ========================================================================
    val retransmitWrite = Output(new RetransmitBufferElement)  // Packet to write to retransmit buffer
    val retransmitWriteValid = Output(Bool())                 // Valid signal for retransmit write
    val retransmitIsFull = Input(Bool())                      // Retransmit buffer full signal

    val isRetransmit = Input(Bool())                          // Currently retransmitting flag

    // ========================================================================
    // Timer - Global timer for timeout and retransmission
    // ========================================================================
    val currTime = Input(UInt(64.W))                          // Current global time

    // ========================================================================
    // ACK Management - Acknowledgment handling
    // ========================================================================
    val ackSeqNum = Input(UInt(22.W))    // Acknowledged sequence number
    val ackType = Input(UInt(2.W))        // Type of ACK (normal, ackonly, etc.)
    val ackReady = Input(Bool())          // ACK data ready signal
    val ackAckonly = Input(Bool())        // ACK-only packet request
    val ackAckonlyDone = Output(Bool())   // ACK-only packet sent

    // ========================================================================
    // Connection Management
    // ========================================================================
    val epConn = Input(Bool())            // Endpoint connection status
    val modeConn = Input(UInt(2.W))      // Connection mode: 0=idle, 1=master, 2=slave

    // ========================================================================
    // Debug Interface
    // ========================================================================
    val debug1 = Input(Bool())            // Debug mode 1: Generate read requests
    val debug2 = Input(Bool())           // Debug mode 2: Generate write requests with ASCII payload
  })

  // ========================================================================
  // Default Output Initialization
  // ========================================================================
  // Initialize TLOEEther interface outputs
  io.txData := 0.U
  io.txStart := false.B
  io.txFlitSize := 0.U

  // Initialize sequence management outputs
  io.incTxSeq := false.B
  io.incRxSeq := false.B
  io.updateAckSeq := false.B
  io.newAckSeq := 0.U

  // Initialize flow control outputs
  io.decCreditValid := false.B
  io.decCreditChannel := 0.U
  io.decCreditAmount := 0.U

  io.decAccCreditValid := false.B
  io.decAccCreditChannel := 0.U
  io.decAccCreditAmount := 0.U

  // Initialize retransmission outputs
  io.retransmitWrite.tloeFrame := 0.U(TLOE_FRAME_SIZE.W)
  io.retransmitWrite.tloeFrameSize := 0.U(5.W)
  io.retransmitWrite.state := 0.U(2.W)
  io.retransmitWrite.sendTime := 0.U(64.W)
  io.retransmitWriteValid := false.B

  // Initialize ACK outputs
  io.ackAckonlyDone := false.B

  // ========================================================================
  // Internal State Registers
  // ========================================================================
  val epConn = RegInit(false.B)
  epConn := io.epConn

  // ========================================================================
  // State Machine Definitions
  // ========================================================================
  // Main transmission state machine (reduced from 12 to 8 states to save LUTs)
  // State merges: txCheckAck+txCheckCredit -> txCheckFrame
  //               txHandleAccCredit+txPrepareSend -> txHandleCredit
  val txIdle :: txAckOnly :: txCheckFrame :: txInitFrame :: txHandleCredit :: txSendPacket :: txEnqRetransmit :: txDone :: Nil = Enum(8)
  val txState = RegInit(txIdle)

  // ACK-only packet state machine
  val aidle :: amakeFrame :: asendRequest :: adone :: Nil = Enum(4)
  val astate = RegInit(aidle)

  // ========================================================================
  // Packet Data Registers - Store TileLink transaction data
  // ========================================================================
  // val tx_size = RegInit(0.U(3.W))  // Removed - unused, saves LUTs

  val nextChan = RegInit(0.U(3.W))      // Next channel to transmit
  val nextOpcode = RegInit(0.U(3.W))    // Next operation code
  val nextParam = RegInit(0.U(4.W))     // Next parameter
  val nextSize = RegInit(0.U(4.W))      // Next transfer size
  val nextSource = RegInit(0.U(26.W))   // Next source ID
  val nextAddr = RegInit(0.U(64.W))     // Next address
  val nextData = RegInit(0.U(512.W))    // Next data payload

  // ========================================================================
  // Control Flags - Indicate what type of packet to send
  // ========================================================================
  val isFrame = RegInit(false.B)         // Frame packet available
  val isACK = RegInit(false.B)          // ACK packet to send
  val isCredit = RegInit(false.B)       // Credit information to send

  // ========================================================================
  // Flow Control Registers
  // ========================================================================
  val maxAccChannel = RegInit(0.U(3.W)) // Channel with maximum accumulated credit
  val maxAccCredit = RegInit(0.U(16.W))  // Maximum accumulated credit value
  maxAccChannel := io.maxCreditChannel
  maxAccCredit := io.maxCredit

  // ========================================================================
  // Packet Generation Registers
  // ========================================================================
  // MASSIVE LUT REDUCTION: Remove frame buffers completely!
  // txFrameVec (4224 bits) and nAckPacketVec (4224 bits) REMOVED
  // Generate packets directly to output instead of buffering
  // This saves ~8500 LUTs from registers + Cat() operations
  
  val txFrameSize = RegInit(0.U(5.W))              // Frame size in flits
  val txRequiredFlits = RegInit(0.U(8.W))          // Required number of flits
  
  // Register for packet storage (needed across multiple cycles/states)
  // Cannot use Wire because packet is generated in txInitFrame and sent in txSendPacket (different cycles)
  val txPacketReg = RegInit(0.U(TLOE_FRAME_SIZE.W))  // Generated TLOE packet to transmit

  // ========================================================================
  // Transmission Control Registers
  // ========================================================================
  // val sendPacket = RegInit(false.B)  // Removed - unused
  val txComplete = RegInit(true.B)      // Transmission complete flag
  // val idx = RegInit(0.U(16.W))       // Removed - unused

  // AXI-Stream registers removed to save LUTs (not used in current design)
  // val axi_txdata = RegInit(0.U(64.W))
  // val axi_txvalid = RegInit(false.B)
  // val axi_txlast = RegInit(false.B)
  // val axi_txkeep = RegInit(0.U(8.W))  

  // ========================================================================
  // Transmission Queue
  // ========================================================================
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

  // ========================================================================
  // Queue Default Values
  // ========================================================================
  // Default queue port values
  txQueue.io.enq.valid := false.B
  txQueue.io.enq.bits := 0.U.asTypeOf(txQueue.io.enq.bits)
  txQueue.io.deq.ready := false.B

  // ========================================================================
  // Debug Mode - Test Data Generation
  // ========================================================================
  // Debug - test read/write address generation
  val testReadAddr = RegInit(0x1000.U(64.W))   // Test read address (increments)
  val testWriteAddr = RegInit(0x1000.U(64.W))  // Test write address (increments)
  
  // ASCII payload messages (512 bits = 64 bytes each)
  // Pad with spaces to exactly 64 bytes for round-robin testing
  def asciiTo512Bits(s: String): UInt = {
    val bytes = s.getBytes("ASCII").take(64)
    val padded = bytes.padTo(64, 0x20.toByte) // Pad with space (0x20)
    val hexString = padded.map(b => f"$b%02x").mkString
    BigInt(hexString, 16).U(512.W)
  }
  
  // 5 different ASCII test messages for round-robin testing
  val payload0 = asciiTo512Bits("1: Hello from OmniXtend! This is test message #1. Testing 512-bit payload.")
  val payload1 = asciiTo512Bits("2: OmniXtend TLOE Protocol Test Message #2. Checking data integrity...")
  val payload2 = asciiTo512Bits("3: Round-robin test payload #3. Verifying packet transmission works.")
  val payload3 = asciiTo512Bits("4: ASCII payload test #4. This message cycles through 5 different texts.")
  val payload4 = asciiTo512Bits("5: Final test message #5. All payloads should be readable ASCII text.")
  
  val payloadVec = VecInit(Seq(payload0, payload1, payload2, payload3, payload4))
  val payloadIndex = RegInit(0.U(3.W))  // 0-4 index for 5 payloads (round-robin)
  
  val testWriteData = payloadVec(payloadIndex)  // Current payload
  val debugEnqueuePending = RegInit(false.B)  // Debug1 enqueue pending flag
  val debug2EnqueuePending = RegInit(false.B)  // Debug2 enqueue pending flag
  
  // ========================================================================
  // ACK Management Registers
  // ========================================================================
  // Simplified: use direct IO instead of intermediate flags to save LUTs
  val ackReadyFlag = RegInit(false.B)  // ACK data ready flag
  // val ackTypeFlag = RegInit(0.U(2.W))  // Removed - use io.ackType directly
  // val ackSeqNumFlag = RegInit(0.U(22.W))  // Removed - use io.ackSeqNum directly

  when(io.ackReady) {
    ackReadyFlag := io.ackReady
    // Use io.ackType and io.ackSeqNum directly instead of buffering
  }

  // ========================================================================
  // Queue Enqueue Logic - Priority: io.tlValid > io.debug1 > io.debug2
  // ========================================================================
  // Enqueue data into the queue when txValid is asserted
  // Priority: io.tlValid > io.debug1 > io.debug2
  when(io.tlValid) {
    txQueue.io.enq.bits.chan := io.tlChan
    txQueue.io.enq.bits.opcode := io.tlOpcode
    txQueue.io.enq.bits.param := io.tlParam
    txQueue.io.enq.bits.size := io.tlSize
    txQueue.io.enq.bits.source := io.tlSource
    txQueue.io.enq.bits.addr := io.tlAddr
    txQueue.io.enq.bits.data := io.tlData
    txQueue.io.enq.valid := true.B
    debugEnqueuePending := false.B
    debug2EnqueuePending := false.B
  }.elsewhen(epConn && io.debug1 && !debugEnqueuePending) {
    when(txQueue.io.enq.ready) {
      txQueue.io.enq.bits.addr := testReadAddr
      txQueue.io.enq.bits.chan := 1.U
      txQueue.io.enq.bits.opcode := 4.U  // GET
      txQueue.io.enq.bits.param := 0.U
      txQueue.io.enq.bits.size := 6.U
      txQueue.io.enq.bits.source := 0.U
      txQueue.io.enq.bits.data := 0.U
      txQueue.io.enq.valid := true.B
      
      testReadAddr := testReadAddr + 0x1000.U
      debugEnqueuePending := true.B
    }
  }.elsewhen(epConn && io.debug2 && !debug2EnqueuePending) {
    when(txQueue.io.enq.ready) {
      txQueue.io.enq.bits.addr := testWriteAddr
      txQueue.io.enq.bits.chan := 1.U
      txQueue.io.enq.bits.opcode := 0.U  // PUTFULLDATA
      txQueue.io.enq.bits.param := 0.U
      txQueue.io.enq.bits.size := 6.U
      txQueue.io.enq.bits.source := 0.U
      txQueue.io.enq.bits.data := testWriteData
      txQueue.io.enq.valid := true.B
      
      testWriteAddr := testWriteAddr + 0x1000.U
      // Round-robin through 5 ASCII payloads
      payloadIndex := Mux(payloadIndex === 4.U, 0.U, payloadIndex + 1.U)
      debug2EnqueuePending := true.B
    }
  }.elsewhen(!io.debug1 && !io.debug2) {
    debugEnqueuePending := false.B
    debug2EnqueuePending := false.B
    txQueue.io.enq.valid := false.B
  }.otherwise {
    txQueue.io.enq.valid := false.B
  }

  // ========================================================================
  // ACK-Only Packet Control
  // ========================================================================
  val ackAckonly = RegInit(false.B)
  ackAckonly := io.ackAckonly
  
  // ========================================================================
  // Flow Control Registers
  // ========================================================================
  // Debug registers removed to save LUTs
  // val creditADecCntDebug = RegInit(0.U(22.W))
  // val maxCreditChannelDebug = RegInit(0.U(3.W))
  // val maxCreditDebug = RegInit(0.U(16.W))

  val txAccChannel = RegInit(0.U(3.W))   // Channel for accumulated credit transmission
  val txAccCredit = RegInit(0.U(16.W))   // Credit value for accumulated credit transmission

  // Removed debug signal assignment to save LUTs
  // when(io.maxCreditChannel =/= 0.U) {
  //   maxCreditChannelDebug := io.maxCreditChannel
  //   maxCreditDebug := io.maxCredit
  // }

  // ========================================================================
  // Helper Functions
  // ========================================================================
  // Initialize next queue item registers to zero
  def initNextQueueItem() = {
    nextChan := 0.U
    nextOpcode := 0.U
    nextParam := 0.U
    nextSize := 0.U
    nextSource := 0.U
    nextAddr := 0.U
    nextData := 0.U
  }

  // ========================================================================
  // Main Transmission State Machine
  // ========================================================================
  switch(txState) {
    // ========================================================================
    // txIdle: Wait for transmission to complete, then check for work
    // ========================================================================
    is(txIdle) {
      initNextQueueItem()

      txAccChannel := 0.U
      txAccCredit := 0.U

      when(txComplete) {
        when(ackAckonly) {
          txState := txAckOnly  // Send ACK-only packet
        }.otherwise {
          txState := txCheckFrame  // Check for frames, ACKs, or credits
        }
      }
    }

    // ========================================================================
    // txAckOnly: Trigger ACK-only packet generation
    // TODO: 처리할 메시지가 있으며 ackonly 프레임을 보내야할까?
    // ========================================================================
    is(txAckOnly) {
      astate := amakeFrame  // Start ACK-only state machine
      txComplete := false.B

      txState := txDone
    }

    // ========================================================================
    // txCheckFrame: Check for frames, ACKs, and credits to send
    // Merged txCheckAck and txCheckCredit to save states
    // ========================================================================
    is(txCheckFrame) {
      // Check for frame in queue
      when (txQueue.io.deq.valid) {
        // First, copy data to next** registers
        nextChan := txQueue.io.deq.bits.chan
        nextOpcode := txQueue.io.deq.bits.opcode
        nextParam := txQueue.io.deq.bits.param
        nextSize := txQueue.io.deq.bits.size
        nextSource := txQueue.io.deq.bits.source
        nextAddr := txQueue.io.deq.bits.addr
        nextData := txQueue.io.deq.bits.data

        isFrame := true.B
        // Dequeue after copying data
        txQueue.io.deq.ready := true.B
      }.otherwise {
        isFrame := false.B
        txQueue.io.deq.ready := false.B
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

    // ========================================================================
    // txInitFrame: Generate TLOE packet based on frame/ACK/credit flags
    // ========================================================================
    is(txInitFrame) {
      when(isFrame) {
        // Generate frame packet and store in register (needed for next cycle)
        txPacketReg := OXPacket.initFrame(nextChan, nextAddr, nextOpcode, nextData, io.nextTxSeq, TLOESeqManager.getPrevSeq(io.nextRxSeq), io.ackType, txAccChannel, txAccCredit, nextSize, nextParam, nextSource)
        txFrameSize := nextSize

        isFrame := false.B
        isACK := false.B
        ackReadyFlag := false.B
        txState := txHandleCredit  // Check credits before sending frame
      }.elsewhen(isACK || isCredit) {
        // Generate ACK packet and store in register
        txPacketReg := OXPacket.normalAck(io.nextTxSeq, TLOESeqManager.getPrevSeq(io.nextRxSeq), 1.U, txAccChannel, txAccCredit)
        txFrameSize := 0.U
        isFrame := false.B
        isACK := false.B
        ackReadyFlag := false.B
        txState := txSendPacket  // Skip credit handling for ACK
      }.otherwise {
        txPacketReg := 0.U
        txState := txDone
      }
    }

    // ========================================================================
    // txHandleCredit: Check and decrement credits before sending
    // Merged txHandleAccCredit, txPrepareSend, txEnqRetransmit to save states
    // ========================================================================
    is(txHandleCredit) {
      val decFlits = TloePacGen.getFlitSize(nextChan, nextOpcode, nextSize)

      when(io.credits(nextChan) >= decFlits) {
        // Sufficient credit available - decrement credit
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
        // Insufficient credit - wait
        io.decCreditValid := false.B
        txState := txHandleCredit  // Wait for credit
      }
    }

    // ========================================================================
    // txSendPacket: Send packet to Ethernet layer and increment sequence number
    // ========================================================================
    is(txSendPacket) {
      // Send from register - no Cat() operation needed!
      io.txData := txPacketReg
      io.txFlitSize := TloePacGen.getFlitSize(nextChan, nextOpcode, nextSize)
      io.txStart := true.B
      io.incTxSeq := true.B  // Increment TX sequence number
      txComplete := false.B
      txState := txEnqRetransmit
    }

    // ========================================================================
    // txEnqRetransmit: Enqueue packet to retransmission buffer
    // ========================================================================
    is(txEnqRetransmit) {
      // Enqueue the packet to the retransmit buffer
      // TODO: check if retransmitter is ready
      io.retransmitWrite.tloeFrame := txPacketReg
      //io.retransmitWrite.tloeFrameSize := txFrameSize
      io.retransmitWrite.tloeFrameSize := TloePacGen.getFlitSize(nextChan, nextOpcode, nextSize)
      io.retransmitWrite.state := 0.U(2.W)  // Initial state
      io.retransmitWrite.sendTime := io.currTime  // Use global timer 

      io.retransmitWriteValid := true.B  // Set valid signal when writing

      txState := txDone
    }

    // ========================================================================
    // txDone: Wait for transmission to complete
    // ========================================================================
    is(txDone) {
      when(txComplete) {
        txState := txIdle
        txComplete := true.B
      }
    }
  }

  // ========================================================================
  // Packet Transmission Completion Logic
  // ========================================================================
  // Packet Sending Logic
  // TODO: need to define as function
  // Debug register removed to save LUTs
  // val debug_txEtherTxReady = RegInit(false.B)
  // debug_txEtherTxReady := io.txReady

  when(io.txReady) {
    // sendPacket := false.B  // Removed
    txComplete := true.B  // Mark transmission as complete
  }

  // ========================================================================
  // ACK-Only Packet State Machine
  // ========================================================================
  // SEND - Packet Send (Ack Only)

  // Debug register removed to save LUTs
  // val debug_nAckPacket = RegInit(0.U(512.W))
  // debug_nAckPacket := nAckPacketAsUInt(4223, 3712)

  // Register for ACK-only packet (needed across states)
  val ackOnlyPacketReg = RegInit(0.U(TLOE_FRAME_SIZE.W))

  // Send an ACKONLY frame
  switch(astate) {
    // ========================================================================
    // amakeFrame: Generate ACK-only packet
    // ========================================================================
    is(amakeFrame) {
      // Generate and store in register
      ackOnlyPacketReg := OXPacket.ackonly(io.nextTxSeq, io.nextRxSeq - 1.U, 1.U, 0.U, 0.U)
      astate := asendRequest
    }

    // ========================================================================
    // asendRequest: Send ACK-only packet to Ethernet layer
    // ========================================================================
    is(asendRequest) {
      // Send from register - no Cat() operation!
      io.txData := ackOnlyPacketReg
      io.txFlitSize := 4.U  // ACK-only packets are 4 flits
      io.txStart := true.B
      txComplete := false.B
      io.ackAckonlyDone := true.B  // Signal ACK-only packet sent
      astate := adone
    }

    // ========================================================================
    // adone: ACK-only packet transmission complete
    // ========================================================================
    is(adone) {
      astate := aidle
    }
  }

  // ========================================================================
  // End of Module
  // ========================================================================
} 