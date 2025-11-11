package omnixtend

import chisel3._
import chisel3.util._

import chisel3.experimental.{IntParam, BaseModule}
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.subsystem.{BaseSubsystem, MBUS}
import freechips.rocketchip.prci.{ClockSinkDomain, ClockSinkParameters}

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

import OmniXtendConstants._

// ========================================================================
// OmniXtend Configuration Parameters
// ========================================================================
/**
 * Configuration parameters for OmniXtend module
 * 
 * @param address Base address for the OmniXtend device
 * @param width Data width (unused in current implementation)
 * @param useAXI4 Whether to use AXI4 interface (unused)
 * @param useBlackBox Whether to use black box implementation (unused)
 */
case class OXParams(
  address: BigInt = 0x1000,
  width: Int = 32,
  useAXI4: Boolean = false,
  useBlackBox: Boolean = true
)

/**
 * Configuration key for OmniXtend parameters
 * Used in Rocket Chip configuration system
 */
case object OXKey extends Field[Option[OXParams]](None)

// ========================================================================
// OmniXtend Bundle Definition
// ========================================================================
/**
 * Definition of OmniXtend Bundle
 * 
 * This bundle defines the interface between OmniXtend and external Ethernet IP.
 * It includes TX/RX data paths and control signals.
 */
class OmniXtendBundle extends Bundle {
  // ========================================================================
  // Status Signal
  // ========================================================================
  val ready       = Output(Bool())  // Signals if the transaction can proceed

  // ========================================================================
  // Ethernet TX Interface - To Ethernet IP
  // ========================================================================
  val txdata      = Output(UInt(512.W))   // 512-bit TX data
  val txvalid     = Output(Bool())        // TX data valid
  val txlast      = Output(Bool())        // Last word in packet
  val txkeep      = Output(UInt(8.W))     // Byte enable mask
  val txready     = Input(Bool())         // TX ready from Ethernet IP

  // ========================================================================
  // Ethernet RX Interface - From Ethernet IP
  // ========================================================================
  val rxdata      = Input(UInt(512.W))    // 512-bit RX data
  val rxvalid     = Input(Bool())          // RX data valid
  val rxlast      = Input(Bool())          // Last word in received packet
//  val rxkeep      = Input(UInt(64.W))     // Unused

  // ========================================================================
  // Control Signals
  // ========================================================================
  val ox_open     = Input(Bool())         // Open connection
  val ox_close    = Input(Bool())         // Close connection
  val debug1      = Input(Bool())         // Debug mode 1 (read requests)
  val debug2      = Input(Bool())         // Debug mode 2 (write requests)
}

/**
 * OmniXtendNode is a LazyModule that defines a TileLink manager node
 * which supports OmniXtend protocol operations. It handles Get and PutFullData
 * requests by interfacing with a Transceiver module.
 * 
 * This module acts as a bridge between TileLink protocol and OmniXtend over Ethernet.
 */
class OmniXtendNode(implicit p: Parameters) extends LazyModule {
  // ========================================================================
  // TileLink Node Configuration
  // ========================================================================
  val beatBytes = 64 // The size of each data beat in bytes
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address            = Seq(AddressSet(0x500000000L, 0x01FFFFFFL)), // Address range this node responds to
    resources          = new SimpleDevice("mem", Seq("example,mem")).reg, // Device resources
    regionType         = RegionType.UNCACHED, // Memory region type
    executable         = true, // Memory is executable
    supportsGet        = TransferSizes(1, beatBytes), // Supported transfer sizes for Get operations
    supportsPutFull    = TransferSizes(1, beatBytes), // Supported transfer sizes for PutFull operations
    supportsPutPartial = TransferSizes(1, beatBytes), // Supported transfer sizes for PutPartial operations
    fifoId             = Some(0) // FIFO ID
  )),
    beatBytes          = 64, // Beat size for the port
    minLatency         = 1 // Minimum latency for the port
  )))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    // ========================================================================
    // IO and Node Connections
    // ========================================================================
    val io  = IO(new OmniXtendBundle) // Input/Output bundle
    val (in, edge) = node.in(0) // Getting the input node and its edge

    // ========================================================================
    // Internal Registers
    // ========================================================================
    // Registers for storing the validity of Get and PutFullData operations
    val aValidReg   = RegInit(false.B) // Register to store the validity of any operation

    // ========================================================================
    // Module Instantiations
    // ========================================================================
    val TLOEEndpoint = Module(new TLOEEndpoint)      // TLOE endpoint module
    val tilelinkHandler = Module(new TileLinkHandler) // TileLink handler module

    val oxAValidReg = RegInit(false.B)  // TileLink A channel valid register
    val oxDValidReg = RegInit(false.B)  // TileLink D channel valid register

    oxAValidReg := in.a.valid
    oxDValidReg := in.d.valid

    // ========================================================================
    // Default TLOE Endpoint Inputs
    // ========================================================================
    TLOEEndpoint.io.tlChan    := 0.U
    TLOEEndpoint.io.tlAddr    := 0.U
    TLOEEndpoint.io.tlData    := 0.U
    TLOEEndpoint.io.tlSize    := 0.U
    TLOEEndpoint.io.tlOpcode  := 0.U
    TLOEEndpoint.io.tlValid   := false.B
    TLOEEndpoint.io.tlMask    := 0.U
    TLOEEndpoint.io.tlSource  := 0.U
    TLOEEndpoint.io.tlParam   := 0.U

    // ========================================================================
    // Module Interconnections
    // ========================================================================
    // Flow control credit signals from TileLink handler to TLOE endpoint
    TLOEEndpoint.io.incAccCreditValid := tilelinkHandler.io.incAccCreditValid
    TLOEEndpoint.io.incAccCreditChannel := tilelinkHandler.io.incAccCreditChannel
    TLOEEndpoint.io.incAccCreditAmount := tilelinkHandler.io.incAccCreditAmount

    // TileLink message signals from TLOE endpoint to TileLink handler
    tilelinkHandler.io.tlMsg              := TLOEEndpoint.io.tlMsg
    tilelinkHandler.io.tlMsgMask          := TLOEEndpoint.io.tlMsgMask
    tilelinkHandler.io.doTilelinkHandler  := TLOEEndpoint.io.doTilelinkHandler

    // ========================================================================
    // Ethernet Interface Connections
    // ========================================================================
    // Connect TLOEEndpoint to external ethernet interface (TX)
    io.txdata   := TLOEEndpoint.io.txdata
    io.txvalid  := TLOEEndpoint.io.txvalid
    io.txlast   := TLOEEndpoint.io.txlast
    io.txkeep   := TLOEEndpoint.io.txkeep

    // Connect external ethernet signals to TLOEEndpoint (RX and control)
    TLOEEndpoint.io.txready  := io.txready

    TLOEEndpoint.io.rxdata   := io.rxdata
    TLOEEndpoint.io.rxvalid  := io.rxvalid
    TLOEEndpoint.io.rxlast   := io.rxlast

    // ========================================================================
    // Control Signal Connections
    // ========================================================================
    // VIO (Virtual I/O) control signals
    TLOEEndpoint.io.ox_open   := io.ox_open
    TLOEEndpoint.io.ox_close  := io.ox_close
    TLOEEndpoint.io.ox_debug1 := io.debug1
    TLOEEndpoint.io.ox_debug2 := io.debug2

    // MAC Address configuration (initialize to 0 for now)
    TLOEEndpoint.io.setSrcMac  := 0.U
    TLOEEndpoint.io.setDestMac := 0.U

/*
    val debug_onInSource = RegInit(0.U(26.W))
    val debug_onInOpcode = RegInit(0.U(3.W))

    val debug_oxMACReg = RegInit(0.U(64.W))
    */

    // ========================================================================
    // TileLink A Channel Processing
    // ========================================================================
    // When the input channel 'a' is ready and valid
    when (in.a.fire) {
      // Transmit the address, data, and opcode from the input channel
      TLOEEndpoint.io.tlChan   := CHANNEL_A
      TLOEEndpoint.io.tlAddr   := in.a.bits.address
      TLOEEndpoint.io.tlData   := in.a.bits.data
      TLOEEndpoint.io.tlSize   := in.a.bits.size
      TLOEEndpoint.io.tlOpcode := in.a.bits.opcode
      TLOEEndpoint.io.tlMask   := in.a.bits.mask
      TLOEEndpoint.io.tlSource := in.a.bits.source
      TLOEEndpoint.io.tlParam  := in.a.bits.param

/*
       debug_onInSource := in.a.bits.source
       debug_onInOpcode := in.a.bits.opcode
       */

      TLOEEndpoint.io.tlValid  := true.B // Mark the transmission as valid
    }

    // Mark the input channel 'a' as valid
    when (in.a.valid) {
        aValidReg := true.B
    }

    // ========================================================================
    // TileLink D Channel Processing
    // ========================================================================
    // Default values for the response channel 'd'
    in.d.valid        := false.B
    in.d.bits.opcode  := 0.U
    in.d.bits.param   := 0.U
    in.d.bits.size    := 0.U
    in.d.bits.source  := 0.U
    in.d.bits.sink    := 0.U
    in.d.bits.denied  := false.B
    in.d.bits.data    := 0.U
    in.d.bits.corrupt := false.B

/*
    // Debug
    val ep_rxData = RegInit(0.U(512.W))
    val ep_rxValid = RegInit(false.B)
    ep_rxData := tilelinkHandler.io.ep_rxData
    ep_rxValid := tilelinkHandler.io.ep_rxValid

    val debug_oxOpcode = RegInit(0.U(3.W))
    val debug_oxParam = RegInit(0.U(4.W))
    val debug_oxSize = RegInit(0.U(4.W))
    val debug_oxSource = RegInit(0.U(26.W))
    val debug_oxDValid = RegInit(false.B)
    debug_oxOpcode := tilelinkHandler.io.ep_rxOpcode
    debug_oxParam := tilelinkHandler.io.ep_rxParam
    debug_oxSize := tilelinkHandler.io.ep_rxSize
    debug_oxSource := tilelinkHandler.io.ep_rxSource
    debug_oxDValid := in.d.valid
    */

    // When TileLink handler has valid RX data
    when (tilelinkHandler.io.ep_rxValid) {
      // RX valid signal received from Ethernet IP
      in.d.valid        := true.B                          // Mark the response as valid
      in.d.bits         := edge.AccessAck(in.a.bits)       // Generate an AccessAck response
      in.d.bits.opcode  := tilelinkHandler.io.ep_rxOpcode  // Set the opcode from the register
      in.d.bits.param   := tilelinkHandler.io.ep_rxParam   // Set the parameter from the register
      in.d.bits.size    := tilelinkHandler.io.ep_rxSize    // Set the size from the register
      in.d.bits.source  := tilelinkHandler.io.ep_rxSource  // Set the source ID from the register
      in.d.bits.sink    := 0.U                             // Set sink to 0
      in.d.bits.denied  := false.B                         // Mark as not denied

      // Optimized with Mux chain instead of switch
      // Handle data response based on opcode and size
      when (tilelinkHandler.io.ep_rxOpcode === D_ACCESSACKDATA_OPCODE) {
        // AccessAckData - extract data based on size
        in.d.bits.data := MuxCase(tilelinkHandler.io.ep_rxData, Seq(
          (tilelinkHandler.io.ep_rxSize === 1.U) -> tilelinkHandler.io.ep_rxData(15, 0),   // 2 bytes
          (tilelinkHandler.io.ep_rxSize === 2.U) -> tilelinkHandler.io.ep_rxData(31, 0),   // 4 bytes
          (tilelinkHandler.io.ep_rxSize === 3.U) -> tilelinkHandler.io.ep_rxData(63, 0),   // 8 bytes
          (tilelinkHandler.io.ep_rxSize === 4.U) -> tilelinkHandler.io.ep_rxData(127, 0), // 16 bytes
          (tilelinkHandler.io.ep_rxSize === 5.U) -> tilelinkHandler.io.ep_rxData(255, 0)   // 32 bytes
        ))
        in.d.bits.corrupt := false.B // Mark as not corrupt
      }.elsewhen (tilelinkHandler.io.ep_rxOpcode === D_ACCESSACK_OPCODE) {
        // AccessAck - no data
        in.d.bits.data    := 0.U
      }
    }

    // ========================================================================
    // Ready Signal Generation
    // ========================================================================
    // Ready conditions for the input channel 'a' and response channel 'd'
    in.a.ready := in.a.valid || aValidReg
    in.d.ready := in.a.valid || aValidReg

    // IO ready signal is asserted when input is valid and opcode is Get or PutFullData
    io.ready := in.a.valid && (in.a.bits.opcode === A_GET_OPCODE || in.a.bits.opcode === A_PUTFULLDATA_OPCODE)
  }
}

// ========================================================================
// OmniXtend Trait for Rocket Chip Integration
// ========================================================================
/**
 * Trait to add OmniXtend to a Rocket Chip subsystem
 * 
 * This trait instantiates the OmniXtend node and connects it to the memory bus (MBUS).
 * It adds necessary adapters (TLBuffer, TLWidthWidget) for proper integration.
 */
trait OmniXtend { this: BaseSubsystem =>
  private val portName = "OmniXtend"
  implicit val p: Parameters

  val ox = LazyModule(new OmniXtendNode()(p))
  
  private val mbus = locateTLBusWrapper(MBUS)

  // Connect OmniXtend node to memory bus with adapters
  mbus.coupleTo(portName) { (ox.node
    :*= TLBuffer()                    // Add buffer for flow control
    :*= TLWidthWidget(mbus.beatBytes) // Width adapter
    :*= _)
  }
}

// ========================================================================
// Configuration Class
// ========================================================================
/**
 * Configuration class to enable OmniXtend in Rocket Chip
 * 
 * @param useAXI4 Whether to use AXI4 interface (unused)
 * @param useBlackBox Whether to use black box implementation (unused)
 */
class WithOX(useAXI4: Boolean = false, useBlackBox: Boolean = false) extends Config((site, here, up) => {
  case OXKey => Some(OXParams(useAXI4 = useAXI4, useBlackBox = useBlackBox))
})
