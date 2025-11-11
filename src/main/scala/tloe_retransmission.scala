package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._
import TloePacGen._

// ========================================================================
// Retransmit Buffer Element Bundle
// ========================================================================
/**
 * RetransmitBufferElement defines the structure of an entry in the retransmission buffer.
 * 
 * Each entry contains:
 * - The TLOE frame to be retransmitted
 * - Frame size in flits
 * - State information
 * - Timestamp when the frame was sent
 */
class RetransmitBufferElement extends Bundle {
  //val tloeFrame = new TloePacket
  val tloeFrame = UInt(TLOE_FRAME_SIZE.W)  // TLOE frame data (768 bits)
  val tloeFrameSize = UInt(5.W)            // Frame size in flits
  val state = UInt(2.W)                     // State information
  val sendTime = UInt(64.W)                // Timestamp when frame was sent
}

// ========================================================================
// Retransmission Module
// ========================================================================
/**
 * Retransmission Module
 * 
 * This module handles packet retransmission for reliable delivery:
 * - Buffers transmitted packets for potential retransmission
 * - Retransmits packets on NAK or timeout
 * - Manages sliding window for acknowledged packets
 * - Tracks send times for timeout detection
 * 
 * Key features:
 * - Queue-based buffer for retransmission candidates
 * - Timeout-based retransmission
 * - Sequence number-based window sliding
 * - State machine for retransmission flow
 */
class Retransmission extends Module {
  val io = IO(new Bundle {
    // ========================================================================
    // Write Interface - From Transmitter
    // ========================================================================
    // Write interface - Transceiver writes packets to retransmit buffer
    val write = Input(new RetransmitBufferElement)  // Packet to buffer
    val writeValid = Input(Bool())                  // Write valid signal
    val writeReady = Output(Bool())                 // Write ready signal (backpressure)
    
    // ========================================================================
    // Read Interface (Unused)
    // ========================================================================
    // Read interface - Retransmission reads packets from buffer
    val read = Input(UInt(TLOE_FRAME_SIZE.W))  // Unused read interface
    
    // ========================================================================
    // Control Interface
    // ========================================================================
    // Control interface
    val clear = Input(Bool())      // Clear buffer
    val isEmpty = Output(Bool())    // Buffer empty flag
    val isFull = Output(Bool())     // Buffer full flag
    val count = Output(UInt(5.W))   // Current buffer count
    
    // ========================================================================
    // Retransmission Interface - From Receiver
    // ========================================================================
    // Retransmission interface
    val retransmitSeqNum = Input(UInt(22.W))  // Sequence number to retransmit
    val retransmitValid = Input(Bool())       // Retransmit request valid
    val retransmitDone = Output(Bool())       // Retransmission complete
    
    // ========================================================================
    // Window Slide Interface - From Receiver
    // ========================================================================
    // Window slide interface
    val slideSeqNumAck = Input(UInt(22.W))  // Acknowledged sequence number
    val slideValid = Input(Bool())          // Slide window request valid
    val slideDone = Output(Bool())          // Window slide complete

    // ========================================================================
    // TLOE Ethernet Interface
    // ========================================================================
    // TLOEEther Interface (simplified)
    val txData = Output(UInt(TLOE_FRAME_SIZE.W))  // TX data
    val txFlitSize = Output(UInt(7.W))            // TX flit size
    val txStart = Output(Bool())                  // TX start signal
    val txReady = Input(Bool())                   // TX ready signal

    // ========================================================================
    // Status Outputs
    // ========================================================================
    val isRetransmit = Output(Bool())  // Retransmission active flag

    // ========================================================================
    // Timer Interface
    // ========================================================================
    val currTime = Input(UInt(64.W))  // Current global time
  })

  // ========================================================================
  // Current Time Register
  // ========================================================================
  val currTime = RegInit(0.U(64.W))
  currTime := io.currTime

  // ========================================================================
  // Retransmission Buffer
  // ========================================================================
  // Main buffer using Queue
  val retransmitBuffer = Module(new Queue(new RetransmitBufferElement, RETRANSMIT_BUFFER_SIZE))

  // ========================================================================
  // Debug Registers
  // ========================================================================
  // Debug
  val rt_debug_rtWValid = RegInit(false.B)
  val rt_debug_currFrame = RegInit(0.U(1024.W))
  val rt_debug_currFrameSize = RegInit(0.U(5.W))
  val rt_debug_currState = RegInit(0.U(2.W))
  val rt_debug_currSendTime = RegInit(0.U(64.W))
  val rt_debug_bufferCount = RegInit(0.U(5.W))

  rt_debug_rtWValid := io.writeValid
  rt_debug_currFrame := retransmitBuffer.io.deq.bits.tloeFrame(TLOE_FRAME_SIZE-1, TLOE_FRAME_SIZE-64)  // Header bits (767, 704)
  rt_debug_currFrameSize := retransmitBuffer.io.deq.bits.tloeFrameSize
  rt_debug_currState := retransmitBuffer.io.deq.bits.state
  rt_debug_currSendTime := retransmitBuffer.io.deq.bits.sendTime
  rt_debug_bufferCount := retransmitBuffer.io.count
 
  // ========================================================================
  // Initialization
  // ========================================================================
  // Initialize RetransmitBufferElement Wire
  val initElement = Wire(new RetransmitBufferElement)
  initElement.tloeFrame := 0.U(TLOE_FRAME_SIZE.W)
  initElement.tloeFrameSize := 0.U(5.W)
  initElement.state := 0.U(2.W)
  initElement.sendTime := 0.U(64.W)
  
  // Initialize Queue signals
  retransmitBuffer.io.enq.valid := false.B
  retransmitBuffer.io.enq.bits := initElement
  retransmitBuffer.io.deq.ready := false.B
  
  // Status signals
  io.isEmpty := retransmitBuffer.io.count === 0.U
  io.isFull := retransmitBuffer.io.count === RETRANSMIT_BUFFER_SIZE.U
  io.count := retransmitBuffer.io.count
  io.retransmitDone := false.B
  io.slideDone := false.B

  // Initialize TLOEEther interface
  io.txData := 0.U
  io.txStart := false.B
  io.txFlitSize := 0.U

  // ========================================================================
  // Enqueue to Retransmit Buffer
  // ========================================================================
  // Sequence number of the last element in the retransmit buffer
  val retransmitBufferLastSeqNum = RegInit(0.U(22.W))

  when(io.writeValid) {
    val writeFrameHeader = io.write.tloeFrame(TLOE_FRAME_SIZE-1, TLOE_FRAME_SIZE-64).asTypeOf(new tloeHeader)  // Header bits (767, 704)
    retransmitBufferLastSeqNum := writeFrameHeader.seqNum

    retransmitBuffer.io.enq.valid := true.B
    retransmitBuffer.io.enq.bits := io.write

    io.writeReady := retransmitBuffer.io.enq.ready
  }.otherwise {
    retransmitBuffer.io.enq.valid := false.B
    io.writeReady := false.B
  }

  // ========================================================================
  // Timeout Check and Retransmission
  // ========================================================================
  val timeoutCheck = RegInit(false.B)
  val timeoutRetransmit = RegInit(false.B)
  val lastTimeoutCheck = RegInit(0.U(64.W))
  val timeoutDelta = RegInit(0.U(64.W))

  val retransmitSendTime = RegInit(0.U(64.W))
  retransmitSendTime := retransmitBuffer.io.deq.bits.sendTime

  // Timeout check logic using Timer's isTimeout function
  when(retransmitBuffer.io.count =/= 0.U) {
    // Check every timeout threshold (100ms at 100MHz)
    timeoutDelta := currTime - lastTimeoutCheck
    when(timeoutDelta >= TIMEOUT_THRESHOLD) {
      timeoutCheck := true.B
      lastTimeoutCheck := currTime
      when(Timer.isTimeout(currTime, retransmitSendTime)) {
        timeoutRetransmit := true.B
        timeoutCheck := false.B
      }
    }.otherwise {
      timeoutRetransmit := false.B
    }
  }.otherwise {
    timeoutCheck := false.B
    timeoutRetransmit := false.B
  }

  // ========================================================================
  // Retransmission State Machine
  // ========================================================================
  val isRetransmit = RegInit(false.B)
  io.isRetransmit := isRetransmit

  // Debug
  val retransmitElement = RegInit(initElement)
  val retransmitElementSeqNum = RegInit(0.U(22.W))
  val rt_debut_rtCnt = RegInit(0.U(10.W))

  // debug
  val rt_debug_rtEFrame = RegInit(0.U(1024.W))
  val rt_debug_rtEFrameSize = RegInit(0.U(5.W))
  val rt_debug_rtESeqNum = RegInit(0.U(22.W))
  val rt_debug_rtEState = RegInit(0.U(2.W))
  val rt_debug_rtESendTime = RegInit(0.U(64.W))

  rt_debug_rtEFrame := retransmitElement.tloeFrame(TLOE_FRAME_SIZE-1, TLOE_FRAME_SIZE-64)  // Header bits (767, 704)
  rt_debug_rtEFrameSize := retransmitElement.tloeFrameSize
  rt_debug_rtESeqNum := retransmitElementSeqNum
  rt_debug_rtEState := retransmitElement.state
  rt_debug_rtESendTime := retransmitElement.sendTime
 
  val rtIdle :: rtDequeue :: rtSend :: rtEnqueue :: rtDone :: Nil = Enum(5)
  val retransmitState = RegInit(rtIdle)

  // In case of NAK or timeout, retransmit the frame in the retransmit buffer
  when (io.retransmitValid || timeoutRetransmit) {
    isRetransmit := true.B
    when (retransmitState === rtIdle) {  // Wait for the idle state
      retransmitState := rtDequeue       // Start with dequeue
    }
  }

  when(isRetransmit) {
    switch(retransmitState) {
      is(rtIdle) {  // Idle state
        retransmitBuffer.io.deq.ready := false.B
        retransmitBuffer.io.enq.valid := false.B
      }
      is(rtDequeue) {  // Dequeue state - Get packet from buffer
        rt_debut_rtCnt := rt_debut_rtCnt + 1.U

        when(retransmitBuffer.io.deq.valid) {
          retransmitElement := retransmitBuffer.io.deq.bits
          retransmitBuffer.io.deq.ready := true.B

          retransmitState := rtSend  // Move to send state
        }.otherwise {
          // Buffer is empty
          isRetransmit := false.B
          retransmitBuffer.io.deq.ready := false.B
          //retransmitState := 0.U
          io.retransmitDone := true.B
        }
      }
      is(rtSend) {  // Send state - Transmit packet
        retransmitBuffer.io.deq.ready := false.B

        when (io.txReady) {
          io.txData := retransmitElement.tloeFrame
          io.txFlitSize := retransmitElement.tloeFrameSize
          io.txStart := true.B

          retransmitState := rtEnqueue
        }
      }
      is(rtEnqueue) {  // Enqueue state - Put packet back in buffer with new timestamp
        // TODO merge with rtSend???
        when(retransmitBuffer.io.enq.ready) { 
          retransmitBuffer.io.enq.valid := true.B
          retransmitBuffer.io.enq.bits := retransmitElement
          retransmitBuffer.io.enq.bits.sendTime := currTime

          retransmitState := rtDone
        }
      }
      is(rtDone) {  // Done state - Check if all packets retransmitted
        val retransmitElementSeqNum = retransmitElement.tloeFrame(TLOE_FRAME_SIZE-1, TLOE_FRAME_SIZE-64).asTypeOf(new tloeHeader).seqNum  // Header bits (767, 704)
        when (TLOESeqManager.seqNumCompare(retransmitBufferLastSeqNum, retransmitElementSeqNum) === 0.S) {
          // All packets retransmitted
          isRetransmit := false.B
          io.retransmitDone := true.B
          timeoutRetransmit := false.B
        }.otherwise {
          // More packets to retransmit
          retransmitState := rtDequeue
        }
      }
   }
  }.otherwise {
    retransmitState := rtIdle
    io.retransmitDone := false.B
  }

  // ========================================================================
  // Slide Window Logic
  // ========================================================================
  // Slide Window logic - Remove acknowledged packets from buffer
  val isSlideWindow = RegInit(false.B)

  val swIdle :: swDeqCmp :: swSlide :: swDone :: Nil = Enum(4)
  val slideWindowState = RegInit(swIdle)

  // Debug
  val rt_slideSeqNumAck = RegInit(0.U(22.W) )

  when(io.slideValid) {
    rt_slideSeqNumAck := io.slideSeqNumAck
    isSlideWindow := true.B

    when (slideWindowState === swIdle) {
      slideWindowState := swDeqCmp
    }
  }

  when (isSlideWindow) {
    switch(slideWindowState) {
      is(swIdle) {  // Idle state
        retransmitBuffer.io.deq.ready := false.B
        retransmitBuffer.io.enq.valid := false.B
      }
      is(swDeqCmp) {  // Dequeue and compare state - Check if packet should be removed
        when(retransmitBuffer.io.deq.valid) {
          val element = retransmitBuffer.io.deq.bits
          val elementSeqNum = element.tloeFrame(TLOE_FRAME_SIZE-1, TLOE_FRAME_SIZE-64).asTypeOf(new tloeHeader).seqNum  // Header bits (767, 704)

          when (TLOESeqManager.seqNumCompare(elementSeqNum, rt_slideSeqNumAck) <= 0.S) {
            // Packet is acknowledged - remove it
            slideWindowState := swSlide  // TODO Merge..??
          }.otherwise {
            // Packet not yet acknowledged - stop sliding
            slideWindowState := swDone
          }
        }.otherwise {
          // Buffer empty - done
          slideWindowState := swDone
        }
      }
      is(swSlide) {  // Slide state - Remove acknowledged packet
        retransmitBuffer.io.deq.ready := true.B
        slideWindowState := swDeqCmp
      }
      is(swDone) {  // Done state - Window slide complete
        isSlideWindow := false.B
        io.slideDone := true.B
        slideWindowState := swIdle
      }
    }
  }

  when(io.clear) {
    retransmitBuffer.reset := true.B
  }

  //////////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////////
  /*
  dontTouch(isRetransmit)
  dontTouch(currTime)

  dontTouch(rt_debug_rtWValid)
  dontTouch(rt_debug_currFrame)
  dontTouch(rt_debug_currFrameSize)
  dontTouch(rt_debug_currSeqNum)
  dontTouch(rt_debug_currState)
  dontTouch(rt_debug_currSendTime)

  dontTouch(rt_debug_bufferCount)

  dontTouch(retransmitSendTime)

  dontTouch(isRetransmit)
  dontTouch(isSlideWindow)

  dontTouch(retransmitBufferLastSeqNum)

  dontTouch(timeoutCheck)
  dontTouch(timeoutRetransmit)
  dontTouch(lastTimeoutCheck)
  dontTouch(timeoutDelta)

  dontTouch(retransmitState)

  dontTouch(rt_slideSeqNumAck)

  dontTouch(slideWindowState)

  dontTouch(rt_debug_rtEFrame)
  dontTouch(rt_debug_rtEFrameSize)
  dontTouch(rt_debug_rtESeqNum)
  dontTouch(rt_debug_rtEState)
  dontTouch(rt_debug_rtESendTime)

  dontTouch(rt_debut_rtCnt)
  */
} 