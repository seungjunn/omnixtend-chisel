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

  val (maxChannel, maxCredit) = getMaxAccumulatedCreditChannelAndValue()
  io.maxCreditChannel := maxChannel
  io.maxCredit := maxCredit

  // Connect credit values to IO
  for (i <- 0 until 6) {
    io.credits(i) := channels(i)
  }

  // Register for storing max credit information
  val debug_maxCreditChannelReg = RegInit(0.U(3.W))
  val debug_maxCreditReg = RegInit(0.U(16.W))

  // Update max credit information every cycle
  val (debugChannel, debugCredit) = getMaxAccumulatedCreditChannelAndValue()
  debug_maxCreditChannelReg := debugChannel
  debug_maxCreditReg := debugCredit

  // Function to find channel with maximum accumulated credit
  // Combined function: returns (channel, outgoing_credit) for the channel with max accumulated credit
  def getMaxAccumulatedCreditChannelAndValue(): (UInt, UInt) = {
    // Find maximum accumulated credit value
    val maxCredit = accChannels.reduceTree((a, b) => Mux(a >= b, a, b))
    
    // Find channel with maximum credit (lowest index if multiple channels have same max)
    // When maxCredit is 0, return channel 0. Otherwise find the first channel with maxCredit
    val maxChannel = Mux(maxCredit === 0.U, 0.U, 
      Mux(accChannels.map(_ === maxCredit).reduce(_ || _), 
        PriorityEncoder(accChannels.map(_ === maxCredit)), 
        0.U))
    
    // Calculate outgoing credit (largest power of 2 for the max credit)
    val outgoingCredit = Mux(maxCredit === 0.U, 0.U, 
      Mux(maxCredit.orR, PriorityEncoder(maxCredit), 0.U))
    
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