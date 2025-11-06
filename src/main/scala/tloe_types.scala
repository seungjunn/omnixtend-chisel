package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

/**
 * EthernetHeader class defines the structure of an Ethernet header.
 */
class EthernetHeader extends Bundle {
// val preamble  = UInt(64.W)    // 8-byte Preamble/SFD
  val destMAC   = UInt(48.W)    // 6-byte Destination MAC Address
  val srcMAC    = UInt(48.W)    // 6-byte Source MAC Address
  val etherType = UInt(16.W)    // 2-byte EtherType field
}

/**
 * OmniXtendHeader class defines the structure of an OmniXtend header.
 * 64 Bits (8 Bytes)
 */
class tloeHeader extends Bundle {
  val vc        = UInt(3.W)     // Virtual Channel
  val msgType   = UInt(4.W)     // Reserved
  val res1      = UInt(3.W)     // Reserved
  val seqNum    = UInt(22.W)    // Sequence Number
  val seqNumAck = UInt(22.W)    // Sequence Number Acknowledgment
  val ack       = UInt(1.W)     // Acknowledgment
  val res2      = UInt(1.W)     // Reserved
  val chan      = UInt(3.W)     // Channel
  val credit    = UInt(5.W)     // Credit
}

/**
 * TileLinkMessage class defines the structure of a TileLink message.
 * 64 Bits (8 Bytes)
 */
class TLMessageHigh extends Bundle {
  val res1      = UInt(1.W)     // Reserved
  val chan      = UInt(3.W)     // Channel
  val opcode    = UInt(3.W)     // Opcode
  val res2      = UInt(1.W)     // Reserved
  val param     = UInt(4.W)     // Parameter
  val size      = UInt(4.W)     // Size
  val domain    = UInt(8.W)     // Domain
  val err       = UInt(2.W)     // Error
  val res3      = UInt(12.W)    // Reserved
  val source    = UInt(26.W)    // Source
}

/**
 * TileLinkMessage class defines the structure of a TileLink message.
 */
class TLMessageLow extends Bundle {
  val addr      = UInt(64.W)    // Address
}

class tloeFrame extends Bundle {
  val tloeHeader  = new tloeHeader
  val tlMsgHigh   = new TLMessageHigh
  val tlMsgLow    = new TLMessageLow
}
/**
 * TloePacket class defines the structure of a TLoE packet.
 */
class TloePacket extends Bundle {
  val ethHeader   = new EthernetHeader
  val tloeHeader  = new tloeHeader
  val tlMsgHigh   = new TLMessageHigh
  val tlMsgLow    = new TLMessageLow
}

object MsgType {
  val NORMAL     = 0.U(4.W)     // Normal message
  val ACKONLY    = 1.U(4.W)     // Acknowledgment only message
  val OPECONN    = 2.U(4.W)     // Open connection message
  val CLOSECONN  = 3.U(4.W)     // Close connection message
}

/**
 * TloePacketGenerator object contains functions to create and manipulate TLoE packets.
 */
object TloePacGen {

  /**
   * Converts a 64-bit unsigned integer from little-endian to big-endian format.
   * @param value A 64-bit UInt to be converted.
   * @return A 64-bit UInt in big-endian format.
   */
  def toBigEndian(value: UInt): UInt = {
    require(value.getWidth == 64, "Input must be 64 bits wide")  // Ensure the input is 64 bits wide

    // Rearrange the bytes of the input value to convert it to big-endian format
    Cat(
      value(7, 0),     // Least significant byte (original bits 7:0)
      value(15, 8),    // Next byte (original bits 15:8)
      value(23, 16),   // Next byte (original bits 23:16)
      value(31, 24),   // Next byte (original bits 31:24)
      value(39, 32),   // Next byte (original bits 39:32)
      value(47, 40),   // Next byte (original bits 47:40)
      value(55, 48),   // Next byte (original bits 55:48)
      value(63, 56)    // Most significant byte (original bits 63:56)
    )
  }

  /**
   * Converts a 512-bit UInt from little-endian to big-endian format.
   * This function processes the input as 8 chunks of 64-bit data, converting each chunk
   * to big-endian and then combining them in the correct order.
   * 
   * @param value A 512-bit UInt to be converted.
   * @return A 512-bit UInt in big-endian format.
   */
  def toBigEndian512(value: UInt): UInt = {
    require(value.getWidth == 512, "Input must be 512 bits wide")  // Ensure the input is 512 bits wide

    // Convert each 64-bit chunk to big-endian
    val chunk0 = toBigEndian(value(63, 0))      // bits 63:0
    val chunk1 = toBigEndian(value(127, 64))    // bits 127:64
    val chunk2 = toBigEndian(value(191, 128))   // bits 191:128
    val chunk3 = toBigEndian(value(255, 192))   // bits 255:192
    val chunk4 = toBigEndian(value(319, 256))   // bits 319:256
    val chunk5 = toBigEndian(value(383, 320))   // bits 383:320
    val chunk6 = toBigEndian(value(447, 384))   // bits 447:384
    val chunk7 = toBigEndian(value(511, 448))   // bits 511:448

    // Combine all chunks in the correct order (chunk7 is most significant)
    Cat(chunk7, chunk6, chunk5, chunk4, chunk3, chunk2, chunk1, chunk0)
  }

  /**
   * Converts a 512-bit UInt from little-endian to big-endian format (CORRECTED VERSION).
   * This function properly reverses both byte order within each 64-bit chunk AND
   * the order of chunks themselves for complete endianness conversion.
   * 
   * @param value A 512-bit UInt to be converted.
   * @return A 512-bit UInt in big-endian format.
   */
  def toBigEndian2(value: UInt): UInt = {
    require(value.getWidth == 512, "Input must be 512 bits wide")  // Ensure the input is 512 bits wide

    // Convert each 64-bit chunk to big-endian (reverse bytes within each chunk)
    val chunk0 = toBigEndian(value(63, 0))      // bits 63:0 (LSB)
    val chunk1 = toBigEndian(value(127, 64))    // bits 127:64
    val chunk2 = toBigEndian(value(191, 128))   // bits 191:128
    val chunk3 = toBigEndian(value(255, 192))   // bits 255:192
    val chunk4 = toBigEndian(value(319, 256))   // bits 319:256
    val chunk5 = toBigEndian(value(383, 320))   // bits 383:320
    val chunk6 = toBigEndian(value(447, 384))   // bits 447:384
    val chunk7 = toBigEndian(value(511, 448))   // bits 511:448 (MSB)

    // Reverse chunk order: place chunk0 (originally LSB) at MSB position
    // This completes the endianness conversion at both byte and chunk level
    Cat(chunk0, chunk1, chunk2, chunk3, chunk4, chunk5, chunk6, chunk7)
  }

/*
  def getSeqNum3(packet: UInt): UInt = {
    val result = packet(4213, 4192)
    result
  }
  */

  // Optimized getFlitSize with nested Mux instead of switch
  def getFlitSize(chan: UInt, opcode: UInt, size: UInt): UInt = {
    Mux(chan === CHANNEL_A,
      Mux(opcode === A_GET_OPCODE, 2.U,
        Mux(opcode === A_PUTFULLDATA_OPCODE, 2.U + ((1.U << size) >> 3.U),
          0.U)),
      Mux(chan === CHANNEL_D,
        Mux(opcode === D_ACCESSACK_OPCODE, 1.U,
          Mux(opcode === D_ACCESSACKDATA_OPCODE, 1.U + ((1.U << size) >> 3.U),
            0.U)),
        0.U))
  }
}
