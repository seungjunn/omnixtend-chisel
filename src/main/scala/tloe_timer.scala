package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

// ========================================================================
// GlobalTimer Module
// ========================================================================
/** GlobalTimer module provides a global timer for the entire system.
  * 
  * This timer is used for tracking packet transmission times and timeout management.
  * It provides a 64-bit counter that increments every clock cycle.
  * 
  * Key features:
  * - 64-bit counter (can count up to 2^64 cycles)
  * - Reset capability
  * - Used by retransmission module for timeout detection
  */
class GlobalTimer extends Module {
  val io = IO(new Bundle {
    val globalTimer = Output(UInt(64.W))  // Global timer output (64-bit counter)
    val resetTimer = Input(Bool())         // Reset timer signal
  })

  // ========================================================================
  // Timer Register
  // ========================================================================
  // 64-bit timer register
  val timer = RegInit(0.U(64.W))

  // ========================================================================
  // Timer Logic
  // ========================================================================
  // Reset timer when resetTimer is asserted
  when(io.resetTimer) {
    timer := 0.U
  }.otherwise {
    timer := timer + 1.U  // Increment every clock cycle
  }

  // Connect timer to output
  io.globalTimer := timer

  //////////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////////
}

// ========================================================================
// Timer Object - Utility Functions
// ========================================================================
object Timer {
  // ========================================================================
  // Time Difference Calculation
  // ========================================================================
  // Helper function to calculate time difference considering wrap-around
  // Handles the case where timer wraps around from max to 0
  def timeDiff(current: UInt, previous: UInt): UInt = {
    Mux(current >= previous,
        current - previous,                    // Normal case: current >= previous
        (current + (1.U << 64)) - previous)    // Wrap-around case: current < previous
  }

  // ========================================================================
  // Timeout Check Function
  // ========================================================================
  // Function to check if timeout has occurred
  // Returns true if the time difference between current and refTime exceeds TIMEOUT_THRESHOLD
  def isTimeout(current: UInt, refTime: UInt): Bool = {
    timeDiff(current, refTime) >= TIMEOUT_THRESHOLD
  }
} 