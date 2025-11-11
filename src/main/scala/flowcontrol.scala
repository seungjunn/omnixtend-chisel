package omnixtend

import chisel3._
import chisel3.util._
import chisel3.dontTouch

import OmniXtendConstants._

// ========================================================================
// Credit Bundle Definition
// ========================================================================
/**
 * CreditBundle defines the interface for credit operations
 * Used for both increment and decrement credit operations
 */
class CreditBundle extends Bundle {
  val valid = Input(Bool())      // Valid signal for credit operation
  val channel = Input(UInt(3.W))  // Channel ID (0-5)
  val credit = Input(UInt(16.W))  // Credit amount
}

// ========================================================================
// FlowControl Module
// ========================================================================
/**
 * FlowControl Module
 * 
 * This module manages credit-based flow control for all TileLink channels.
 * It maintains:
 * - Regular credits for each channel (for flow control)
 * - Accumulated credits for each channel (for credit reporting)
 * 
 * Key features:
 * - Supports simultaneous increment and decrement operations
 * - Finds channel with maximum accumulated credit
 * - Optimized for LUT reduction
 */
class FlowControl extends Module {
  val io = IO(new Bundle {
    // ========================================================================
    // Credit Management Interfaces
    // ========================================================================
    // Credit management signals
    val incCredit = new CreditBundle    // Increment regular credit
    val decCredit = new CreditBundle    // Decrement regular credit

    val incAccCredit = new CreditBundle // Increment accumulated credit
    val decAccCredit = new CreditBundle // Decrement accumulated credit

    // ========================================================================
    // Credit Status Outputs
    // ========================================================================
    val credits = Output(Vec(6, UInt(16.W)))  // Current credits for each channel
    val maxCreditChannel = Output(UInt(3.W))  // Channel with maximum accumulated credit
    val maxCredit = Output(UInt(16.W))        // Maximum accumulated credit value (power of 2)
  })

  // ========================================================================
  // Credit Registers
  // ========================================================================
  // Credit registers for each channel (vectorized)
  val channels = RegInit(VecInit(Seq.fill(6)(INIT_CREDIT.U(16.W))))
  
  // Accumulated credit registers for each channel (vectorized)
  val accChannels = RegInit(VecInit(Seq.fill(6)(0.U(16.W))))

  // ========================================================================
  // Maximum Credit Calculation
  // ========================================================================
  // Calculate max credit only once per cycle to save LUTs
  val (maxChannel, maxCredit) = getMaxAccumulatedCreditChannelAndValue()
  io.maxCreditChannel := maxChannel
  io.maxCredit := maxCredit

  // ========================================================================
  // Credit Output Connection
  // ========================================================================
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

  // ========================================================================
  // Helper Functions
  // ========================================================================
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

  // ========================================================================
  // Regular Credit Update Logic
  // ========================================================================
  // Handle simultaneous increment and decrement
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
    // Increment only
    val creditAmount = io.incCredit.credit
    // Update regular credit
    channels(io.incCredit.channel) := channels(io.incCredit.channel) + creditAmount
  }.elsewhen(io.decCredit.valid) {
    // Decrement only
    val creditAmount = io.decCredit.credit
    // Update regular credit
    channels(io.decCredit.channel) := channels(io.decCredit.channel) - creditAmount 
  }

  // ========================================================================
  // Accumulated Credit Update Logic
  // ========================================================================
  // Handle simultaneous increment and decrement for accumulated credits
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
    // Increment accumulated credit only
    val creditAmount = io.incAccCredit.credit

    // Update accumulated credit
    accChannels(io.incAccCredit.channel) := accChannels(io.incAccCredit.channel) + creditAmount
  }.elsewhen(io.decAccCredit.valid) {
    // Decrement accumulated credit only
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