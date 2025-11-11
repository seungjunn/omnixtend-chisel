package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

/**
 * TLOESeqManager Module
 * 
 * This module manages sequence numbers for reliable packet delivery:
 * - TX sequence numbers: Sequence numbers for transmitted packets
 * - RX sequence numbers: Expected sequence numbers for received packets
 * - ACK sequence numbers: Last acknowledged sequence number
 * 
 * Key features:
 * - Handles sequence number wrap-around (22-bit, max 0x3FFFFF)
 * - Updates sequence numbers atomically
 * - Provides sequence number comparison utilities
 */
class TLOESeqManager extends Module {
  val io = IO(new Bundle {
    // ========================================================================
    // Input Signals
    // ========================================================================
    // Input signals
    val incTxSeq = Input(Bool())        // Increment TX sequence number
    val incRxSeq = Input(Bool())        // Increment RX sequence number
    val updateAckSeq = Input(Bool())    // Update ACK sequence number
    val newAckSeq = Input(UInt(22.W))   // New ACK sequence number to set
    val reset = Input(Bool())            // Reset sequence numbers

    // ========================================================================
    // Output Signals
    // ========================================================================
    // Output signals
    val nextTxSeq = Output(UInt(22.W))  // Current TX sequence number
    val nextRxSeq = Output(UInt(22.W))  // Current RX sequence number
    val ackdSeq = Output(UInt(22.W))    // Current ACK sequence number
  })

  // ========================================================================
  // Sequence Number Registers
  // ========================================================================
  val nextTxSeq = RegInit(0.U(22.W))              // TX sequence number (starts at 0)
  val nextRxSeq = RegInit(0.U(22.W))              // RX sequence number (starts at 0)
  val ackdSeq = RegInit(MAX_SEQ_NUM.U(22.W))      // ACK sequence number (starts at max for wrap-around detection)

  val checkInput = RegInit(0.U(22.W))  // Debug register

  // ========================================================================
  // Helper Functions
  // ========================================================================
  // Function to get nextRxSeq minus 1
  def getPrevNextRxSeq(): UInt = {
    Mux(nextRxSeq === 0.U, MAX_SEQ_NUM.U, nextRxSeq - 1.U)
  }

  // ========================================================================
  // Sequence Number Update Logic
  // ========================================================================
  // Reset logic
  when(io.reset) {
    nextTxSeq := 0.U
    nextRxSeq := 0.U
    ackdSeq := MAX_SEQ_NUM.U
  }.otherwise {
    // Increment TX sequence number
    when(io.incTxSeq) {
      nextTxSeq := (nextTxSeq + 1.U) & MAX_SEQ_NUM.U  // Wrap around at max
    }

    // Increment RX sequence number
    when(io.incRxSeq) {
      nextRxSeq := (nextRxSeq + 1.U) & MAX_SEQ_NUM.U  // Wrap around at max
    }

    // Update ackd sequence number only when new value is greater
    when(io.updateAckSeq) {
      when(TLOESeqManager.seqNumCompare(io.newAckSeq, ackdSeq) > 0.S) {
        ackdSeq := io.newAckSeq
      }
    }
  }

  // ========================================================================
  // Output Connections
  // ========================================================================
  // Connect outputs
  io.nextTxSeq := nextTxSeq
  io.nextRxSeq := nextRxSeq
  io.ackdSeq := ackdSeq

  //////////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////////
  dontTouch(nextTxSeq)
  dontTouch(nextRxSeq)
  dontTouch(ackdSeq)
  dontTouch(checkInput)
}

// ========================================================================
// TLOESeqManager Object - Utility Functions
// ========================================================================
object TLOESeqManager {
  // ========================================================================
  // Sequence Number Comparison Function
  // ========================================================================
  // Optimized sequence number comparison with wrap around handling
  // Reduced LUT usage by simplifying logic
  // Returns: 1.S if seq1 > seq2, -1.S if seq1 < seq2, 0.S if seq1 == seq2
  def seqNumCompare(seq1: UInt, seq2: UInt): SInt = {
    val halfMaxSeq = (1 << 21).U  // 2^21

    // Calculate signed difference with wrap-around consideration
    val rawDiff = (seq1 - seq2).asSInt
    val absDiff = Mux(seq1 >= seq2, seq1 - seq2, seq2 - seq1)

    // Simplified comparison logic
    MuxCase(0.S, Seq(
      (seq1 === seq2) -> 0.S,
      (absDiff <= halfMaxSeq && seq1 > seq2) -> 1.S,
      (absDiff <= halfMaxSeq && seq1 < seq2) -> (-1).S,
      (absDiff > halfMaxSeq && seq1 > seq2) -> (-1).S,
      (absDiff > halfMaxSeq && seq1 < seq2) -> 1.S
    ))
  }

  // ========================================================================
  // Sequence Number Navigation Functions
  // ========================================================================
  /**
   * Calculates the previous sequence number in the sequence space.
   * Handles wrap-around: if seq is 0, returns MAX_SEQ_NUM
   * 
   * @param seq Current sequence number
   * @return Previous sequence number (with wrap-around)
   */
  def getPrevSeq(seq: UInt): UInt = {
    (seq - 1.U) & MAX_SEQ_NUM.U
  }

  /**
   * Calculates the next sequence number in the sequence space.
   * Handles wrap-around: if seq is MAX_SEQ_NUM, returns 0
   * 
   * @param seq Current sequence number
   * @return Next sequence number (with wrap-around)
   */
  def getNextSeq(seq: UInt): UInt = {
    (seq + 1.U) & MAX_SEQ_NUM.U
  }
}