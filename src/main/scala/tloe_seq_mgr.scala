package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

class TLOESeqManager extends Module {
  val io = IO(new Bundle {
    // Input signals
    val incTxSeq = Input(Bool())  // Increment TX sequence number
    val incRxSeq = Input(Bool())  // Increment RX sequence number
    val updateAckSeq = Input(Bool())  // Update ACK sequence number
    val newAckSeq = Input(UInt(22.W))  // New ACK sequence number to set
    val reset = Input(Bool())  // Reset sequence numbers

    // Output signals
    val nextTxSeq = Output(UInt(22.W))  // Current TX sequence number
    val nextRxSeq = Output(UInt(22.W))  // Current RX sequence number
    val ackdSeq = Output(UInt(22.W))  // Current ACK sequence number
  })

  val nextTxSeq = RegInit(0.U(22.W))
  val nextRxSeq = RegInit(0.U(22.W))
  val ackdSeq = RegInit(MAX_SEQ_NUM.U(22.W))

  val checkInput = RegInit(0.U(22.W))

  // Function to get nextRxSeq minus 1
  def getPrevNextRxSeq(): UInt = {
    Mux(nextRxSeq === 0.U, MAX_SEQ_NUM.U, nextRxSeq - 1.U)
  }

  // Reset logic
  when(io.reset) {
    nextTxSeq := 0.U
    nextRxSeq := 0.U
    ackdSeq := MAX_SEQ_NUM.U
  }.otherwise {
    // Increment TX sequence number
    when(io.incTxSeq) {
      nextTxSeq := (nextTxSeq + 1.U) & MAX_SEQ_NUM.U
    }

    // Increment RX sequence number
    when(io.incRxSeq) {
      nextRxSeq := (nextRxSeq + 1.U) & MAX_SEQ_NUM.U
    }

    // Update ackd sequence number only when new value is greater
    when(io.updateAckSeq) {
      when(TLOESeqManager.seqNumCompare(io.newAckSeq, ackdSeq) > 0.S) {
        ackdSeq := io.newAckSeq
      }
    }
  }

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

object TLOESeqManager {
  // Optimized sequence number comparison with wrap around handling
  // Reduced LUT usage by simplifying logic
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

  /**
   * Calculates the previous sequence number in the sequence space.
   * @param seq Current sequence number
   * @return Previous sequence number
   */
  def getPrevSeq(seq: UInt): UInt = {
    (seq - 1.U) & MAX_SEQ_NUM.U
  }

  /**
   * Calculates the next sequence number in the sequence space.
   * @param seq Current sequence number
   * @return Next sequence number
   */
  def getNextSeq(seq: UInt): UInt = {
    (seq + 1.U) & MAX_SEQ_NUM.U
  }
}