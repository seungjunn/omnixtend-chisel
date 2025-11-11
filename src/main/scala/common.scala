package omnixtend

import chisel3._
import chisel3.util._

// ========================================================================
// OmniXtend Constants Object
// ========================================================================
/**
 * OmniXtendConstants object contains all constants used throughout the OmniXtend system.
 * 
 * This includes:
 * - Packet size definitions
 * - Protocol constants (ACK/NAK, message types)
 * - Sequence number limits
 * - Channel definitions
 * - TileLink opcode definitions
 */
object OmniXtendConstants {
  // ========================================================================
  // Packet Size Configuration
  // ========================================================================
  // MASSIVE LUT REDUCTION: Reduce max flits from 64 to 10
  // 64 flits = 4224 bits → 10 flits = 768 bits (81% reduction!)
  // This reduces ALL large packet buffers proportionally
  val TLOE_MAX_FLITS = 10  // = 10

  // TILELINK_SIZE: 64bit * 8개(payload) + 64bit * 2개(header) = 10 * 64 = 640 bits
  val TOTAL_TILELINK_SIZE = (TLOE_MAX_FLITS * 64)  // = 10 * 64 = 640 bits
  // TLOE_FRAME_SIZE: 64비트 * 12개 = 768 bits (TILELINK_SIZE + TLOE_HEADER + MASK)
  val TLOE_FRAME_SIZE = ((TLOE_MAX_FLITS+2) * 64)  // = (10+2) * 64 = 12 * 64 = 768 bits
  // 이더넷 헤더: SRC MAC(48) + DEST MAC(48) + ETHER_TYPE(16) = 112 bits
  val TLOE_ETHER_HEADER_SIZE = (48 + 48 + 16)  // = 112 bits
  // TLOE_PACKET_SIZE: TLOE_FRAME_SIZE + 이더넷 헤더
  val TLOE_PACKET_SIZE = TLOE_ETHER_HEADER_SIZE + TLOE_FRAME_SIZE  // = 112 + 768 = 880 bits

  // ========================================================================
  // TLOE Protocol Constants
  // ========================================================================
  val TLOE_NAK = 0.U(1.W)  // Negative Acknowledgment
  val TLOE_ACK = 1.U(1.W)  // Acknowledgment

  val TLOE_TYPE_NORMAL = 0.U(2.W)   // Normal message type
  val TLOE_TYPE_ACKONLY = 1.U(2.W)  // ACK-only message type
  val TLOE_TYPE_OPEN = 2.U(2.W)     // Open connection message type
  val TLOE_TYPE_CLOSE = 3.U(2.W)    // Close connection message type

  // ========================================================================
  // Sequence Number Configuration
  // ========================================================================
  val MAX_SEQ_NUM = 0x3FFFFF  // 22비트 최대값 (4,194,303)
  val HALF_MAX_SEQ_NUM = MAX_SEQ_NUM / 2  // Half of max for wrap-around detection

  // ========================================================================
  // Connection Configuration
  // ========================================================================
  val CHANNEL_NUM = 6  // Number of TileLink channels
  val CREDIT_DEFAULT = 9  //TODO: Default credit value
  //val CREDIT_DEFAULT = 20  // Alternative default (unused)
  val CONN_PACKET_SIZE = 72  // Connection packet size in bytes
  val CONN_RESEND_TIME = 500000000L  // 5 seconds at 100MHz clock
  
  // ========================================================================
  // Message Type Constants
  // ========================================================================
  val TYPE_NORMAL = 0.U(4.W)          // Normal message type
  val TYPE_ACKONLY = 1.U(4.W)         // ACK-only message type
  val TYPE_OPEN_CONNECTION = 2.U(4.W) // Open connection message type
  val TYPE_CLOSE_CONNECTION = 3.U(4.W) // Close connection message type

  // ========================================================================
  // Connection Mode Constants
  // ========================================================================
  val MODE_IDLE = 0.U(2.W)    // Idle mode (no connection)
  val MODE_MASTER = 1.U(2.W)  // Master mode
  val MODE_SLAVE = 2.U(2.W)   // Slave mode
  
  // ========================================================================
  // TileLink Channel Constants
  // ========================================================================
  val CHANNEL_0 = 0.U(3.W)  // Channel 0 (unused)
  val CHANNEL_A = 1.U(3.W)  // Channel A (Acquire)
  val CHANNEL_B = 2.U(3.W)    // Channel B (Probe)
  val CHANNEL_C = 3.U(3.W)    // Channel C (Release)
  val CHANNEL_D = 4.U(3.W)    // Channel D (Grant/AccessAck)
  val CHANNEL_E = 5.U(3.W)    // Channel E (GrantAck)

  // ========================================================================
  // Flow Control Constants
  // ========================================================================
  val INIT_CREDIT = 512      // Initial credit value for channels
  val ERROR_CREDIT = 0xFFFF  // Error value when credit is insufficient

  // ========================================================================
  // TileLink Opcode Constants - Channel D (Grant/AccessAck)
  // ========================================================================
  val D_ACCESSACK_OPCODE = 0.U(3.W)        // AccessAck (no data)
  val D_ACCESSACKDATA_OPCODE = 1.U(3.W)    // AccessAckData (with data)
  val D_HINTACK_OPCODE = 2.U(3.W)          // HintAck
  val D_GRANT_OPCODE = 4.U(3.W)            // Grant (no data)
  val D_GRANTDATA_OPCODE = 5.U(3.W)        // GrantData (with data)
  val D_RELEASEACK_OPCODE = 6.U(3.W)       // ReleaseAck

  // ========================================================================
  // TileLink Opcode Constants - Channel A (Acquire)
  // ========================================================================
  val A_PUTFULLDATA_OPCODE = 0.U(3.W)      // PutFullData
  val A_PUTPARTIALDATA_OPCODE = 1.U(3.W)   // PutPartialData
  val A_ARITHMETICDATA_OPCODE = 2.U(3.W)   // ArithmeticData
  val A_LOGICALDATA_OPCODE = 3.U(3.W)      // LogicalData
  val A_GET_OPCODE = 4.U(3.W)              // Get

  // ========================================================================
  // TileLink Opcode Constants - Channel B (Probe)
  // ========================================================================
  val B_PUTFULLDATA_OPCODE = 0.U(3.W)      // PutFullData
  val B_PUTPARTIALDATA_OPCODE = 1.U(3.W)   // PutPartialData
  val B_ARITHMETICDATA_OPCODE = 2.U(3.W)   // ArithmeticData
  val B_LOGICALDATA_OPCODE = 3.U(3.W)      // LogicalData
  val B_GET_OPCODE = 4.U(3.W)              // Get

  // ========================================================================
  // TileLink Opcode Constants - Channel C (Release)
  // ========================================================================
  val C_ACCESSACK_OPCODE = 0.U(3.W)        // AccessAck
  val C_ACCESSACKDATA_OPCODE = 1.U(3.W)    // AccessAckData
  val C_PROBEACKDATA_OPCODE = 5.U(3.W)     // ProbeAckData
  val C_RELEASEDATA_OPCODE = 6.U(3.W)      // ReleaseData

  // ========================================================================
  // MAC Address Registers (Unused - defined for compatibility)
  // ========================================================================
  val srcMac = RegInit(0.U(48.W))
  val destMac = RegInit(0.U(48.W))

  // ========================================================================
  // Timer Configuration
  // ========================================================================
  //val TIMEOUT_THRESHOLD = 1000000000.U(64.W) // 10s at 100MHz clock (unused)
  val TIMEOUT_THRESHOLD = 10000000.U(64.W) // 100ms at 100MHz clock
  val RETRANSMIT_BUFFER_SIZE = 2  // Size of retransmission buffer

  // ========================================================================
  // Queue Configuration
  // ========================================================================
  // TileLink Handler
  val TL_QUEUE_DEPTH = 2  // Number of packets that can be queued for transmission
} 
