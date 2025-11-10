package omnixtend

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import OmniXtendConstants._

class TileLinkHandler extends Module {
  val io = IO(new Bundle {
    val tlMsg = Input(UInt(4096.W))
    val tlMsgMask = Input(UInt(64.W))
    val doTilelinkHandler = Input(Bool())

    // Output signals to TLOEEndpoint
    val ep_rxChan = Output(UInt(3.W))
    val ep_rxOpcode = Output(UInt(3.W))
    val ep_rxParam = Output(UInt(4.W))
    val ep_rxSize = Output(UInt(4.W))
    val ep_rxSource = Output(UInt(26.W))
    val ep_rxAddr = Output(UInt(64.W))
    val ep_rxData = Output(UInt(512.W))
    val ep_rxValid = Output(Bool())
    val ep_rxMask = Output(UInt(64.W))

    val tlHandlerReady = Output(Bool())

    val incAccCreditValid = Output(Bool())
    val incAccCreditChannel = Output(UInt(3.W))
    val incAccCreditAmount = Output(UInt(5.W))

  })

  io.incAccCreditValid := false.B
  io.incAccCreditChannel := 0.U
  io.incAccCreditAmount := 0.U

  // TileLink Message Queue
  val tlMsgQueue = Module(new Queue(new Bundle {
    val tlMsg = UInt(4096.W)
    val tlMsgMask = UInt(64.W)
  }, TL_QUEUE_DEPTH)) // 8개의 메시지를 저장할 수 있는 Queue

  // Queue 연결 - io.doTilelinkHandler가 true일 때만 enqueue
  tlMsgQueue.io.enq.valid := io.doTilelinkHandler
  tlMsgQueue.io.enq.bits.tlMsg := io.tlMsg
  tlMsgQueue.io.enq.bits.tlMsgMask := io.tlMsgMask
  
  // Queue가 가득 찰 때 backpressure 제공
  io.tlHandlerReady := tlMsgQueue.io.enq.ready
  
  // Removed debug signals to save LUTs
  // val enqueueSuccess = io.doTilelinkHandler && tlMsgQueue.io.enq.ready
  // val queueCount = tlMsgQueue.io.count

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

  val tlIdle :: tlGetMask :: tlGetTlHeader :: tlGetTlPayload :: tlHandle :: tlDone :: Nil = Enum(6)
  val tlHandlerState = RegInit(tlIdle)

  val tlHeader = Reg(new TLMessageHigh)
  val tlHeaderLow = Reg(new TLMessageLow)
  // val tlHeaderData = Reg(UInt(512.W))  // Removed - not used, saves 512 LUTs

  // MASSIVE LUT REDUCTION: Use single register instead of Vec
  // Removes Cat() and Vec overhead
  val tlMsg = Reg(UInt(4096.W))
  val tlMsgMask = Reg(UInt(64.W))

  val mask = Reg(UInt(64.W))
  val offset = Reg(UInt(6.W))

  // Debug registers and complex extractBits removed to save LUTs
  // Use direct bit slicing instead of dynamic extractBits function

  // TileLink Handler - Direct register assignment, no Vec
  when(tlHandlerState === tlIdle && tlMsgQueue.io.deq.valid) {
    tlMsg := tlMsgQueue.io.deq.bits.tlMsg
    tlMsgMask := tlMsgQueue.io.deq.bits.tlMsgMask
    tlMsgQueue.io.deq.ready := true.B
    tlHandlerState := tlGetMask
  }.otherwise {
    tlMsgQueue.io.deq.ready := false.B
  }

  switch(tlHandlerState) {
    is(tlIdle) {    
      // Initialize control signals in idle state
    }

    // TODO: Currently only handling single tilelink message case
    is(tlGetMask) {
      // Find first set bit in mask (LSB first)
      mask := tlMsgMask
      offset := PriorityEncoder(tlMsgMask)  // Remove Reverse to search from LSB

      tlHandlerState := tlGetTlHeader
    }

    // Use shift operations for dynamic bit extraction
    is(tlGetTlHeader) {
      // Shift to align target bits to LSB, then extract fixed width
      val shiftAmount = TOTAL_TILELINK_SIZE.U - ((offset + 1.U) * 64.U)
      tlHeader := (tlMsg >> shiftAmount)(63, 0).asTypeOf(new TLMessageHigh)
      
      val shiftAmount2 = TOTAL_TILELINK_SIZE.U - ((offset + 2.U) * 64.U)
      tlHeaderLow := (tlMsg >> shiftAmount2)(63, 0).asTypeOf(new TLMessageLow)
      
      tlHandlerState := tlGetTlPayload
    }

    // TODO: Rename to incAccCredit
    is(tlGetTlPayload) {
      io.incAccCreditValid := true.B
      io.incAccCreditChannel := tlHeader.chan
      io.incAccCreditAmount := TloePacGen.getFlitSize(tlHeader.chan, tlHeader.opcode, tlHeader.size)

      tlHandlerState := tlHandle
    }

    is(tlHandle) {
      // Save return values
      rxOpcodeReg := tlHeader.opcode
      rxParamReg := tlHeader.param
      rxSizeReg := tlHeader.size
      rxSourceReg := tlHeader.source
      rxAddrReg := tlHeaderLow.addr

      // Channel별 처리
      switch(tlHeader.chan) {
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
              
              rxDataReg := MuxLookup(tlHeader.size, 0.U)(Seq(
                0.U -> dataChunk(7, 0),
                1.U -> dataChunk(15, 0),
                2.U -> dataChunk(31, 0),
                3.U -> dataChunk(63, 0),
                4.U -> dataChunk(127, 0),
                5.U -> dataChunk(255, 0),
                6.U -> dataChunk(511, 0)
              ))
            }
          }
          rxValidReg := true.B
          tlHandlerState := tlDone
        }
        
        // Channel E (Client -> Manager)
        is(CHANNEL_E) {
          // Grant 응답 처리
          rxDataReg := tlHeaderLow.addr
          rxValidReg := true.B

          tlHandlerState := tlDone
        }
      }
    }

    is(tlDone) {
      rxValidReg := false.B

      tlHandlerState := tlIdle
    }
  }

  //////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////
  /*
  dontTouch(tlHandlerState)
  dontTouch(tlHeader)
  dontTouch(tlHeaderLow)
  dontTouch(tlHeaderData)
  dontTouch(tlMsg)
  dontTouch(tlMsgMask)
  dontTouch(mask)
  dontTouch(offset)
  dontTouch(debug_tl1)
  dontTouch(debug_tl2)
  dontTouch(debug_tl3)
  dontTouch(debug_tlReadResult)
  dontTouch(debug_tlReadResult_valid)
  dontTouch(debug_tlWriteResult)
  dontTouch(debug_tlWriteResult_valid)
  dontTouch(rxChanReg)
  dontTouch(rxOpcodeReg)
  dontTouch(rxParamReg)
  dontTouch(rxSizeReg)
  dontTouch(rxSourceReg)
  dontTouch(rxAddrReg)
  dontTouch(rxDataReg)
  dontTouch(rxValidReg)
  dontTouch(rxMaskReg)

  dontTouch(debug_incAccCreditAmount)
  dontTouch(debug_incAccCreditChannel)
  dontTouch(debug_incAccCreditValid)
  */
} 
