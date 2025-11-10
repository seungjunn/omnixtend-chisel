package omnixtend

import chisel3._
import chisel3.util._
import chisel3.dontTouch

import OmniXtendConstants._

class CreditBundle extends Bundle {
  val valid = Input(Bool())
  val channel = Input(UInt(3.W))
  val credit = Input(UInt(16.W))
}

class FlowControl extends Module {
  val io = IO(new Bundle {
    // Credit management signals
    val incCredit = new CreditBundle
    val decCredit = new CreditBundle

    val incAccCredit = new CreditBundle
    val decAccCredit = new CreditBundle

    val credits = Output(Vec(6, UInt(16.W)))  // Current credits for each channel
    val maxCreditChannel = Output(UInt(3.W))  // Channel with maximum credit
    val maxCredit = Output(UInt(16.W))  // Added for the new maxCredit signal
  })

  // Credit registers for each channel (vectorized)
  val channels = RegInit(VecInit(Seq.fill(6)(INIT_CREDIT.U(16.W))))
  
  // Accumulated credit registers for each channel (vectorized)
  val accChannels = RegInit(VecInit(Seq.fill(6)(0.U(16.W))))

  // Calculate max credit only once per cycle to save LUTs
  val (maxChannel, maxCredit) = getMaxAccumulatedCreditChannelAndValue()
  io.maxCreditChannel := maxChannel
  io.maxCredit := maxCredit

  // Connect credit values to IO
  for (i <- 0 until 6) {
    io.credits(i) := channels(i)
  }

  // Debug registers removed to save LUTs and eliminate redundant computation
  // val debug_maxCreditChannelReg = RegInit(0.U(3.W))
  // val debug_maxCreditReg = RegInit(0.U(16.W))

  // Removed redundant call to getMaxAccumulatedCreditChannelAndValue()
  // val (debugChannel, debugCredit) = getMaxAccumulatedCreditChannelAndValue()
  // debug_maxCreditChannelReg := debugChannel
  // debug_maxCreditReg := debugCredit

  // Optimized function to find channel with maximum accumulated credit
  // Reduced LUT usage by simplifying logic
  def getMaxAccumulatedCreditChannelAndValue(): (UInt, UInt) = {
    // Find maximum accumulated credit value using reduce instead of reduceTree for simplicity
    val maxCredit = accChannels.reduce((a, b) => Mux(a >= b, a, b))
    
    // Find channel with maximum credit - simplified logic
    // When maxCredit is 0, return channel 0
    val maxChannel = Mux(maxCredit === 0.U, 0.U, 
      PriorityEncoder(VecInit(accChannels.map(_ === maxCredit))))
    
    // Calculate outgoing credit (find MSB position for largest power of 2)
    // Use Log2 which is simpler and uses fewer LUTs than bit reversal
    val outgoingCredit = Mux(maxCredit === 0.U, 0.U, 
      Mux(maxCredit.orR, Log2(maxCredit), 0.U))
    
    (maxChannel, outgoingCredit)
  }

  when(io.incCredit.valid && io.decCredit.valid) {
    val creditIncAmount = io.incCredit.credit
    val creditDecAmount = io.decCredit.credit

    // Create vectors for increment and decrement amounts
    val creditIncVec = VecInit(Seq(
      Mux(io.incCredit.channel === 0.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 1.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 2.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 3.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 4.U, creditIncAmount, 0.U),
      Mux(io.incCredit.channel === 5.U, creditIncAmount, 0.U)
    ))

    val creditDecVec = VecInit(Seq(
      Mux(io.decCredit.channel === 0.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 1.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 2.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 3.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 4.U, creditDecAmount, 0.U),
      Mux(io.decCredit.channel === 5.U, creditDecAmount, 0.U)
    ))

    // Update credits for each channel
    for (i <- 0 until 6) {
      channels(i) := channels(i) + creditIncVec(i) - creditDecVec(i)
    }

  }.elsewhen(io.incCredit.valid) {
    val creditAmount = io.incCredit.credit
    // Update regular credit
    channels(io.incCredit.channel) := channels(io.incCredit.channel) + creditAmount
  }.elsewhen(io.decCredit.valid) {
    val creditAmount = io.decCredit.credit
    // Update regular credit
    channels(io.decCredit.channel) := channels(io.decCredit.channel) - creditAmount 
  }

  when(io.incAccCredit.valid && io.decAccCredit.valid) {
    val creditIncAmount = io.incAccCredit.credit
    val creditDecAmount = io.decAccCredit.credit

    val creditAccIncVec = VecInit(Seq(
      Mux(io.incAccCredit.channel === 0.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 1.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 2.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 3.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 4.U, creditIncAmount, 0.U),
      Mux(io.incAccCredit.channel === 5.U, creditIncAmount, 0.U)
    ))

    val creditAccDecVec = VecInit(Seq(
      Mux(io.decAccCredit.channel === 0.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 1.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 2.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 3.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 4.U, creditDecAmount, 0.U),
      Mux(io.decAccCredit.channel === 5.U, creditDecAmount, 0.U)
    ))

    for (i <- 0 until 6) {
      accChannels(i) := accChannels(i) + creditAccIncVec(i) - creditAccDecVec(i)
    }

  }.elsewhen(io.incAccCredit.valid) {
    val creditAmount = io.incAccCredit.credit

    // Update accumulated credit
    accChannels(io.incAccCredit.channel) := accChannels(io.incAccCredit.channel) + creditAmount
  }.elsewhen(io.decAccCredit.valid) {
    val creditAmount = io.decAccCredit.credit

    // Update accumulated credit
    accChannels(io.decAccCredit.channel) := accChannels(io.decAccCredit.channel) - creditAmount
  }

  //////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////
  /*
  for (i <- 0 until 6) {
    dontTouch(channels(i))
    dontTouch(accChannels(i))
  }

  dontTouch(debug_maxCreditChannelReg)
  dontTouch(debug_maxCreditReg)
  */
}