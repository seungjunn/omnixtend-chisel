package omnixtend

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import OmniXtendConstants._

/**
 * TileLinkHandler Module
 * 
 * This module processes TileLink messages received from TLOE packets.
 * It extracts TileLink protocol information and routes it to the appropriate
 * endpoint based on channel and opcode.
 * 
 * Key features:
 * - Queues incoming TileLink messages
 * - Extracts TileLink headers and payloads
 * - Handles different TileLink channels (A, B, C, D, E)
 * - Manages accumulated credits for flow control
 * - Optimized for LUT reduction (uses single register instead of Vec)
 */
class TileLinkHandler extends Module {
  val io = IO(new Bundle {
    // ========================================================================
    // Input Interface - From TLOE Receiver
    // ========================================================================
    val tlMsg = Input(UInt(4096.W))           // TileLink message data (up to 64 flits * 64 bits)
    val tlMsgMask = Input(UInt(64.W))         // Mask indicating which flits contain valid data
    val doTilelinkHandler = Input(Bool())     // Trigger signal to process TileLink message

    // ========================================================================
    // Output Interface - To TLOE Endpoint
    // ========================================================================
    // Output signals to TLOEEndpoint
    val ep_rxChan = Output(UInt(3.W))        // Channel ID
    val ep_rxOpcode = Output(UInt(3.W))      // Operation code
    val ep_rxParam = Output(UInt(4.W))       // Parameter field
    val ep_rxSize = Output(UInt(4.W))        // Transfer size
    val ep_rxSource = Output(UInt(26.W))     // Source ID
    val ep_rxAddr = Output(UInt(64.W))       // Address
    val ep_rxData = Output(UInt(512.W))      // Data payload
    val ep_rxValid = Output(Bool())          // Valid signal
    val ep_rxMask = Output(UInt(64.W))       // Byte enable mask

    val tlHandlerReady = Output(Bool())         // Ready signal (backpressure)

    // ========================================================================
    // Flow Control Interface
    // ========================================================================
    val incAccCreditValid = Output(Bool())      // Increment accumulated credit valid
    val incAccCreditChannel = Output(UInt(3.W)) // Channel for accumulated credit
    val incAccCreditAmount = Output(UInt(5.W))  // Amount of accumulated credit

  })

  // ========================================================================
  // Default Output Initialization
  // ========================================================================
  io.incAccCreditValid := false.B
  io.incAccCreditChannel := 0.U
  io.incAccCreditAmount := 0.U

  // ========================================================================
  // TileLink Message Queue
  // ========================================================================
  // TileLink Message Queue
  val tlMsgQueue = Module(new Queue(new Bundle {
    val tlMsg = UInt(4096.W)
    val tlMsgMask = UInt(64.W)
  }, TL_QUEUE_DEPTH)) // 2개의 메시지를 저장할 수 있는 Queue

  // Queue 연결 - io.doTilelinkHandler가 true일 때만 enqueue
  tlMsgQueue.io.enq.valid := io.doTilelinkHandler
  tlMsgQueue.io.enq.bits.tlMsg := io.tlMsg
  tlMsgQueue.io.enq.bits.tlMsgMask := io.tlMsgMask
  
  // Queue가 가득 찰 때 backpressure 제공
  io.tlHandlerReady := tlMsgQueue.io.enq.ready
  
  // Removed debug signals to save LUTs
  // val enqueueSuccess = io.doTilelinkHandler && tlMsgQueue.io.enq.ready
  // val queueCount = tlMsgQueue.io.count

  // ========================================================================
  // Output Registers
  // ========================================================================
  // TileLink Handler
  val rxChanReg = RegInit(0.U(3.W))
  val rxOpcodeReg = RegInit(0.U(3.W))
  val rxParamReg = RegInit(0.U(4.W))
  val rxSizeReg = RegInit(0.U(4.W))
  val rxSourceReg = RegInit(0.U(26.W))
  val rxAddrReg = RegInit(0.U(64.W))
  val rxDataReg = RegInit(0.U(512.W))
  val rxValidReg = RegInit(false.B)
  val rxMaskReg = RegInit(0.U(64.W))

  // Output signals to TLOEEndpoint
  io.ep_rxChan := rxChanReg
  io.ep_rxOpcode := rxOpcodeReg
  io.ep_rxParam := rxParamReg
  io.ep_rxSize := rxSizeReg
  io.ep_rxSource := rxSourceReg
  io.ep_rxAddr := rxAddrReg
  io.ep_rxData := rxDataReg
  io.ep_rxValid := rxValidReg
  io.ep_rxMask := rxMaskReg

  // ========================================================================
  // State Machine
  // ========================================================================
  val tlIdle :: tlGetMask :: tlGetTlHeader :: tlGetTlPayload :: tlHandle :: tlDone :: Nil = Enum(6)
  val tlHandlerState = RegInit(tlIdle)

  // ========================================================================
  // TileLink Header Registers
  // ========================================================================
  val tlHeader = Reg(new TLMessageHigh)
  val tlHeaderLow = Reg(new TLMessageLow)
  // val tlHeaderData = Reg(UInt(512.W))  // Removed - not used, saves 512 LUTs

  // ========================================================================
  // Message Buffer Registers
  // ========================================================================
  // MASSIVE LUT REDUCTION: Use single register instead of Vec
  // Removes Cat() and Vec overhead
  val tlMsg = Reg(UInt(4096.W))
  val tlMsgMask = Reg(UInt(64.W))

  val mask = Reg(UInt(64.W))
  val offset = Reg(UInt(6.W))

  // Debug registers and complex extractBits removed to save LUTs
  // Use direct bit slicing instead of dynamic extractBits function

  // ========================================================================
  // Queue Dequeue Logic
  // ========================================================================
  // TileLink Handler - Direct register assignment, no Vec
  when(tlHandlerState === tlIdle && tlMsgQueue.io.deq.valid) {
    tlMsg := tlMsgQueue.io.deq.bits.tlMsg
    tlMsgMask := tlMsgQueue.io.deq.bits.tlMsgMask
    tlMsgQueue.io.deq.ready := true.B
    tlHandlerState := tlGetMask
  }.otherwise {
    tlMsgQueue.io.deq.ready := false.B
  }

  // ========================================================================
  // State Machine Logic
  // ========================================================================
  switch(tlHandlerState) {
    is(tlIdle) {    
      // Initialize control signals in idle state
    }

    // ========================================================================
    // State: Get Mask - Find first valid message in mask
    // ========================================================================
    // TODO: Currently only handling single tilelink message case
    is(tlGetMask) {
      // Find first set bit in mask (LSB first)
      mask := tlMsgMask
      offset := PriorityEncoder(tlMsgMask)  // Remove Reverse to search from LSB

      tlHandlerState := tlGetTlHeader
    }

    // ========================================================================
    // State: Get TileLink Header - Extract header from message
    // ========================================================================
    // Use shift operations for dynamic bit extraction
    is(tlGetTlHeader) {
      // Shift to align target bits to LSB, then extract fixed width
      val shiftAmount = TOTAL_TILELINK_SIZE.U - ((offset + 1.U) * 64.U)
      tlHeader := (tlMsg >> shiftAmount)(63, 0).asTypeOf(new TLMessageHigh)
      
      val shiftAmount2 = TOTAL_TILELINK_SIZE.U - ((offset + 2.U) * 64.U)
      tlHeaderLow := (tlMsg >> shiftAmount2)(63, 0).asTypeOf(new TLMessageLow)
      
      tlHandlerState := tlGetTlPayload
    }

    // ========================================================================
    // State: Get TileLink Payload - Update accumulated credits
    // ========================================================================
    // TODO: Rename to incAccCredit
    is(tlGetTlPayload) {
      io.incAccCreditValid := true.B
      io.incAccCreditChannel := tlHeader.chan
      io.incAccCreditAmount := TloePacGen.getFlitSize(tlHeader.chan, tlHeader.opcode, tlHeader.size)

      tlHandlerState := tlHandle
    }

    // ========================================================================
    // State: Handle - Process TileLink message based on channel and opcode
    // ========================================================================
    is(tlHandle) {
      // Save return values
      rxOpcodeReg := tlHeader.opcode
      rxParamReg := tlHeader.param
      rxSizeReg := tlHeader.size
      rxSourceReg := tlHeader.source
      rxAddrReg := tlHeaderLow.addr

      // Channel별 처리
      switch(tlHeader.chan) {
        // ========================================================================
        // Channel A (Acquire) - Memory -> Host
        // ========================================================================
        // Channel A (Memory -> Host)
        is(CHANNEL_A) {
          switch(tlHeader.opcode) {
            is(A_PUTFULLDATA_OPCODE) { // PutFullData
            }
            is(A_PUTPARTIALDATA_OPCODE) { // PutPartialData
            }
            is(A_GET_OPCODE) { // Get
            }
          }
        }

        // ========================================================================
        // Channel B (Probe)
        // ========================================================================
        // TODO: Channel B
        is(CHANNEL_B) {
          switch(tlHeader.opcode) {
            is(B_PUTFULLDATA_OPCODE) { // PutFullData
              rxDataReg := tlHeaderLow.addr
              rxValidReg := true.B

              tlHandlerState := tlDone
            }
            is(B_GET_OPCODE) { // Get
              rxDataReg := tlHeaderLow.addr
              rxValidReg := true.B

              tlHandlerState := tlDone
            }
          }
        }

        // ========================================================================
        // Channel C (Release)
        // ========================================================================
        // TODO: Channel C 
        is(CHANNEL_C) {
          switch(tlHeader.opcode) {
            is(C_ACCESSACK_OPCODE) { // AccessAck
              rxDataReg := tlHeaderLow.addr
              rxValidReg := true.B

              tlHandlerState := tlDone
            }
            is(D_ACCESSACKDATA_OPCODE) { // AccessAckData
              rxDataReg := tlHeaderLow.addr
              rxValidReg := true.B

              tlHandlerState := tlDone
            }
          }
        }

        // ========================================================================
        // Channel D (Grant/AccessAck) - Host -> Memory
        // ========================================================================
        // Channel D (Host -> Memory)
        is(CHANNEL_D) {
          switch(tlHeader.opcode) {
            is(D_ACCESSACK_OPCODE) { // AccessAck
              rxDataReg := 0.U // AccessAck는 데이터가 없음
            }
            is(D_ACCESSACKDATA_OPCODE) { // AccessAckData
              // Use shift for dynamic extraction
              val shiftAmount = TOTAL_TILELINK_SIZE.U - ((offset + 2.U) * 64.U)
              val dataChunk = (tlMsg >> shiftAmount)(511, 0)
              
              // Extract data based on transfer size
              rxDataReg := MuxLookup(tlHeader.size, 0.U)(Seq(
                0.U -> dataChunk(7, 0),      // 1 byte
                1.U -> dataChunk(15, 0),     // 2 bytes
                2.U -> dataChunk(31, 0),     // 4 bytes
                3.U -> dataChunk(63, 0),     // 8 bytes
                4.U -> dataChunk(127, 0),    // 16 bytes
                5.U -> dataChunk(255, 0),    // 32 bytes
                6.U -> dataChunk(511, 0)    // 64 bytes
              ))
            }
          }
          rxValidReg := true.B
          tlHandlerState := tlDone
        }
        
        // ========================================================================
        // Channel E (GrantAck) - Client -> Manager
        // ========================================================================
        // Channel E (Client -> Manager)
        is(CHANNEL_E) {
          // Grant 응답 처리
          rxDataReg := tlHeaderLow.addr
          rxValidReg := true.B

          tlHandlerState := tlDone
        }
      }
    }

    // ========================================================================
    // State: Done - Complete processing and return to idle
    // ========================================================================
    is(tlDone) {
      rxValidReg := false.B

      tlHandlerState := tlIdle
    }
  }

  //////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////
} 
