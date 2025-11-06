package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

/** GlobalTimer module provides a global timer for the entire system.
  * This timer is used for tracking packet transmission times and timeout management.
  */
class GlobalTimer extends Module {
  val io = IO(new Bundle {
    val globalTimer = Output(UInt(64.W))  // Global timer output
    val resetTimer = Input(Bool())         // Reset timer signal
  })

  // 64-bit timer register
  val timer = RegInit(0.U(64.W))

  // Reset timer when resetTimer is asserted
  when(io.resetTimer) {
    timer := 0.U
  }.otherwise {
    timer := timer + 1.U
  }

  // Connect timer to output
  io.globalTimer := timer

  //////////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////////
  /*
  dontTouch(timer)
  */
}

object Timer {
  // Helper function to calculate time difference considering wrap-around
  def timeDiff(current: UInt, previous: UInt): UInt = {
    Mux(current >= previous,
        current - previous,
        (current + (1.U << 64)) - previous)
  }

  // Function to check if timeout has occurred
  def isTimeout(current: UInt, refTime: UInt): Bool = {
    timeDiff(current, refTime) >= TIMEOUT_THRESHOLD
  }
} 