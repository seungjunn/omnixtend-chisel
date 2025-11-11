package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

object OXPacket {
  /** Creates a normal acknowledgment (ACK) packet.
    *
    * @param seq
    *   Sequence number for the packet.
    * @param seq_ack
    *   Sequence number being acknowledged.
    * @param ack
    *   Acknowledgment flag.
    * @param chan
    *   Channel ID.
    * @param credit
    *   Updated credit.
    * @return
    *   A UInt representing the full packet with padding.
    */
  def normalAck(seq: UInt, seq_ack: UInt, ack: UInt, chan: UInt, credit: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloeFrame = Wire(new tloeFrame)

    // Populate the OmniXtend header fields
    tloeFrame.tloeHeader.vc := 0.U // Virtual Channel ID
    tloeFrame.tloeHeader.msgType := 0.U // Message Type 0 (Normal)
    tloeFrame.tloeHeader.res1 := 0.U // Reserved field 1
    tloeFrame.tloeHeader.seqNum := seq // Sequence Number (0)
    tloeFrame.tloeHeader.seqNumAck := seq_ack // Acknowledged Sequence Number (2^22-1)
    tloeFrame.tloeHeader.ack := ack // Acknowledgment flag
    tloeFrame.tloeHeader.res2 := 0.U // Reserved field 2
    tloeFrame.tloeHeader.chan := chan // Channel ID
    tloeFrame.tloeHeader.credit := credit // Credit field

    // Populate the high part of the TileLink message fields
    tloeFrame.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloeFrame.tlMsgHigh.chan := 0.U // Channel ID
    tloeFrame.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
    tloeFrame.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloeFrame.tlMsgHigh.param := 0.U // TileLink parameter field
    tloeFrame.tlMsgHigh.size := 0.U // Size of the transaction
    tloeFrame.tlMsgHigh.domain := 0.U // Domain field
    tloeFrame.tlMsgHigh.err := 0.U // Error field
    tloeFrame.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloeFrame.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloeFrame.tlMsgLow.addr := 0.U // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    //val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))
    val packetWithPadding = Cat(tloeFrame.asUInt, 0.U((TLOE_FRAME_SIZE - tloeFrame.asUInt.getWidth).W))

    packetWithPadding
  }

  def ackonly(seq: UInt, seq_ack: UInt, ack: UInt, chan: UInt, credit: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloeFrame = Wire(new tloeFrame)

    // Populate the OmniXtend header fields
    tloeFrame.tloeHeader.vc := 0.U // Virtual Channel ID
    tloeFrame.tloeHeader.msgType := 1.U // Message Type 1 (Ack Only)
    tloeFrame.tloeHeader.res1 := 0.U // Reserved field 1
    tloeFrame.tloeHeader.seqNum := seq // Sequence Number (0)
    tloeFrame.tloeHeader.seqNumAck := seq_ack // Acknowledged Sequence Number (2^22-1)
    tloeFrame.tloeHeader.ack := ack // Acknowledgment flag
    tloeFrame.tloeHeader.res2 := 0.U // Reserved field 2
    tloeFrame.tloeHeader.chan := chan // Channel ID
    tloeFrame.tloeHeader.credit := credit // Credit field

    // Populate the high part of the TileLink message fields
    tloeFrame.tlMsgHigh.res1 := 0.U // Reserved field 1
    tloeFrame.tlMsgHigh.chan := 0.U // Channel ID
    tloeFrame.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
    tloeFrame.tlMsgHigh.res2 := 0.U // Reserved field 2
    tloeFrame.tlMsgHigh.param := 0.U // TileLink parameter field
    tloeFrame.tlMsgHigh.size := 0.U // Size of the transaction
    tloeFrame.tlMsgHigh.domain := 0.U // Domain field
    tloeFrame.tlMsgHigh.err := 0.U // Error field
    tloeFrame.tlMsgHigh.res3 := 0.U // Reserved field 3
    tloeFrame.tlMsgHigh.source := 0.U // Source field

    // Populate the low part of the TileLink message fields
    tloeFrame.tlMsgLow.addr := 0.U // TileLink address (input parameter)

    // Convert the TLoE packet bundle to a single UInt representing the entire packet
    //val packetWithPadding = Cat(tloePacket.asUInt, 0.U(272.W))
    val packetWithPadding = Cat(tloeFrame.asUInt, 0.U((TLOE_FRAME_SIZE - tloeFrame.asUInt.getWidth).W))

    packetWithPadding
  }

  def initFrame(txChan: UInt, txAddr: UInt, txOpcode: UInt, txData: UInt, seqNum: UInt, seqNumAck: UInt, ackType: UInt, char: UInt, credit: UInt, size: UInt, param: UInt, source: UInt): UInt = {
    // Create a new instance of the TloePacket (a user-defined bundle)
    val tloeFrame = Wire(new tloeFrame)
    
    // packetWithPadding 초기화 수정
    val packetWithPadding = WireInit(0.U(TLOE_FRAME_SIZE.W))

    // Populate the OmniXtend header fields
    tloeFrame.tloeHeader.vc := 0.U // Virtual Channel ID
    tloeFrame.tloeHeader.msgType := 0.U // Message Type 0 (Normal)
    tloeFrame.tloeHeader.res1 := 0.U // Reserved field 1
    tloeFrame.tloeHeader.seqNum := seqNum // Sequence Number
    tloeFrame.tloeHeader.seqNumAck := seqNumAck // Acknowledged Sequence Number
    tloeFrame.tloeHeader.ack := ackType // Acknowledgment flag
    tloeFrame.tloeHeader.res2 := 0.U // Reserved field 2
    tloeFrame.tloeHeader.chan := char // Channel ID
    tloeFrame.tloeHeader.credit := credit // Credit field

    // txOpcode 비교 수정
    when(txChan === CHANNEL_A && txOpcode === A_PUTFULLDATA_OPCODE) {
      // Populate the high part of the TileLink message fields
      tloeFrame.tlMsgHigh.res1 := 0.U // Reserved field 1
      tloeFrame.tlMsgHigh.chan := txChan // Channel ID (A)
      tloeFrame.tlMsgHigh.opcode := txOpcode // TileLink operation code (input parameter)
      tloeFrame.tlMsgHigh.res2 := 0.U // Reserved field 2
      tloeFrame.tlMsgHigh.param := param // TileLink parameter field
      tloeFrame.tlMsgHigh.size := size // Size of the transaction
      tloeFrame.tlMsgHigh.domain := 0.U // Domain field
      tloeFrame.tlMsgHigh.err := 0.U // Error field
      tloeFrame.tlMsgHigh.res3 := 0.U // Reserved field 3
      tloeFrame.tlMsgHigh.source := source // Source field

      // Populate the low part of the TileLink message fields
      tloeFrame.tlMsgLow.addr := txAddr
      //tloePacket.tlMsgLow.addr := txData(63, 0)
      //tloePacket.tlMsgLow.addr := txData(511, 448)


      // Define Padding and Mask
      val mask = "h0000000000000001".U(64.W) // 64-bit mask, all bits set to 1

      // packetWithPadding 정의
      // TLOE_FRAME_SIZE = 768 bits, tloeFrame = 192 bits
      // Format: [tloeFrame(192)][data][mask(64)][padding to 768]
      switch(size) {
        is(5.U) {  // 32 Bytes = 256 bits
          // 192 + 256 + 64 + 256 = 768
          packetWithPadding := Cat(tloeFrame.asUInt, txData(255, 0), mask, 0.U(256.W))
        }
        is(6.U) {  // 64 Bytes = 512 bits
          // 192 + 512 + 64 + 0 = 768
          packetWithPadding := Cat(tloeFrame.asUInt, txData(511, 0), mask)
        }
      }
    }.elsewhen(txChan === CHANNEL_A && txOpcode === A_GET_OPCODE) {
      tloeFrame.tlMsgHigh.res1 := 0.U // Reserved field 1
      tloeFrame.tlMsgHigh.chan := txChan // Channel ID (A)
      tloeFrame.tlMsgHigh.opcode := txOpcode // TileLink operation code (input parameter)
      tloeFrame.tlMsgHigh.res2 := 0.U // Reserved field 2
      tloeFrame.tlMsgHigh.param := param // TileLink parameter field
      tloeFrame.tlMsgHigh.size := size // Size of the transaction
      tloeFrame.tlMsgHigh.domain := 0.U // Domain field
      tloeFrame.tlMsgHigh.err := 0.U // Error field
      tloeFrame.tlMsgHigh.res3 := 0.U // Reserved field 3
      tloeFrame.tlMsgHigh.source := source // Source field

      // Populate the low part of the TileLink message fields
      tloeFrame.tlMsgLow.addr := txAddr // TileLink address (input parameter)

      // Define Padding and Mask
      val mask = "h0000000000000001".U(64.W) // 64-bit mask, all bits set to 1

      // GET has no data payload, just header + mask + padding
      // 192 + 128 + 64 + 384 = 768
      packetWithPadding := Cat(tloeFrame.asUInt, 0.U(128.W), mask, 0.U(384.W))

    }.elsewhen(txChan === CHANNEL_D && txOpcode === D_ACCESSACK_OPCODE) {  // AccessAck
      tloeFrame.tlMsgHigh.res1 := 0.U // Reserved field 1
      tloeFrame.tlMsgHigh.chan := txChan // Channel ID (A)
      tloeFrame.tlMsgHigh.opcode := txOpcode // TileLink operation code (input parameter)
      tloeFrame.tlMsgHigh.res2 := 0.U // Reserved field 2
      tloeFrame.tlMsgHigh.param := param // TileLink parameter field
      tloeFrame.tlMsgHigh.size := size // Size of the transaction
      tloeFrame.tlMsgHigh.domain := 0.U // Domain field
      tloeFrame.tlMsgHigh.err := 0.U // Error field
      tloeFrame.tlMsgHigh.res3 := 0.U // Reserved field 3
      tloeFrame.tlMsgHigh.source := source // Source field

      // Populate the low part of the TileLink message fields
      tloeFrame.tlMsgLow.addr := 0.U // TileLink address (input parameter)

      // Define Padding and Mask
      val mask = "h0000000000000001".U(64.W) // 64-bit mask, all bits set to 1

      // AccessAck has no data payload
      // 192 + 128 + 64 + 384 = 768
      packetWithPadding := Cat(tloeFrame.asUInt, 0.U(128.W), mask, 0.U(384.W))

    }.elsewhen(txChan === CHANNEL_D && txOpcode === D_ACCESSACKDATA_OPCODE) {  // AccessAckData
      tloeFrame.tlMsgHigh.res1 := 0.U // Reserved field 1
      tloeFrame.tlMsgHigh.chan := txChan // Channel ID (A)
      tloeFrame.tlMsgHigh.opcode := txOpcode // TileLink operation code (input parameter)
      tloeFrame.tlMsgHigh.res2 := 0.U // Reserved field 2
      tloeFrame.tlMsgHigh.param := param // TileLink parameter field
      tloeFrame.tlMsgHigh.size := size // Size of the transaction
      tloeFrame.tlMsgHigh.domain := 0.U // Domain field
      tloeFrame.tlMsgHigh.err := 0.U // Error field
      tloeFrame.tlMsgHigh.res3 := 0.U // Reserved field 3
      tloeFrame.tlMsgHigh.source := source // Source field

      // Populate the low part of the TileLink message fields
      tloeFrame.tlMsgLow.addr := 0.U // Not used

      // Define Padding and Mask
      val mask = "h0000000000000001".U(64.W) // 64-bit mask, all bits set to 1

      // Data response packet
      switch(size) {
        is(6.U) {  // 64 Bytes = 512 bits
          // Format: [tloeHeader+tlMsgHigh(128)][data(512)][mask(64)][padding(64)] = 768
          // Skip tlMsgLow.addr (not used for data response)
          packetWithPadding := Cat(tloeFrame.asUInt(191, 64), txData(511, 0), mask, 0.U(64.W))
        }
      }
 
 /*
      //TODO
      packetWithPadding := Cat(tloePacket.asUInt(303, 64), txData, mask, 0.U(3520.W), 0.U(16.W))
*/
    }.otherwise {
      tloeFrame.tlMsgHigh.res1 := 0.U // Reserved field 1
      tloeFrame.tlMsgHigh.chan := 0.U // Channel ID (A)
      tloeFrame.tlMsgHigh.opcode := 0.U // TileLink operation code (input parameter)
      tloeFrame.tlMsgHigh.res2 := 0.U // Reserved field 2
      tloeFrame.tlMsgHigh.param := 0.U // TileLink parameter field
      tloeFrame.tlMsgHigh.size := 0.U // Size of the transaction
      tloeFrame.tlMsgHigh.domain := 0.U // Domain field
      tloeFrame.tlMsgHigh.err := 0.U // Error field
      tloeFrame.tlMsgHigh.res3 := 0.U // Reserved field 3
      tloeFrame.tlMsgHigh.source := 0.U // Source field

      // Populate the low part of the TileLink message fields
      tloeFrame.tlMsgLow.addr := 0.U // TileLink address (input parameter)

      // Define Padding and Mask
      val mask = "h0000000000000000".U(64.W) // 64-bit mask, all bits set to 0

      // Otherwise case: just header + padding
      // 192 + 576 = 768
      packetWithPadding := Cat(tloeFrame.asUInt, 0.U(576.W))
    }
    packetWithPadding
  }
  //////////////////////////////////////////////////////////////
  // DEBUG
  //////////////////////////////////////////////////////////////
}