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

case class OXParams(
  address: BigInt = 0x1000,
  width: Int = 32,
  useAXI4: Boolean = false,
  useBlackBox: Boolean = true
)

case object OXKey extends Field[Option[OXParams]](None)

// Definition of OmniXtend Bundle
class OmniXtendBundle extends Bundle {
  val ready       = Output(Bool())  // signals if the transaction can proceed

  // Connected to Ethernet IP
  val txdata      = Output(UInt(512.W))
  val txvalid     = Output(Bool())
  val txlast      = Output(Bool())
  val txkeep      = Output(UInt(8.W))
  val txready     = Input(Bool())

  val rxdata      = Input(UInt(512.W))
  val rxvalid     = Input(Bool())
  val rxlast      = Input(Bool())
//  val rxkeep      = Input(UInt(64.W))

  val ox_open     = Input(Bool())
  val ox_close    = Input(Bool())
  val debug1      = Input(Bool())
  val debug2      = Input(Bool())
}

/**
 * OmniXtendNode is a LazyModule that defines a TileLink manager node
 * which supports OmniXtend protocol operations. It handles Get and PutFullData
 * requests by interfacing with a Transceiver module.
 */
class OmniXtendNode(implicit p: Parameters) extends LazyModule {
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
    val io  = IO(new OmniXtendBundle) // Input/Output bundle
    val (in, edge) = node.in(0) // Getting the input node and its edge

    // Registers for storing the validity of Get and PutFullData operations
    val aValidReg   = RegInit(false.B) // Register to store the validity of any operation

    val TLOEEndpoint = Module(new TLOEEndpoint)
    val tilelinkHandler = Module(new TileLinkHandler)

    val oxAValidReg = RegInit(false.B)
    val oxDValidReg = RegInit(false.B)

    oxAValidReg := in.a.valid
    oxDValidReg := in.d.valid

    TLOEEndpoint.io.tlChan    := 0.U
    TLOEEndpoint.io.tlAddr    := 0.U
    TLOEEndpoint.io.tlData    := 0.U
    TLOEEndpoint.io.tlSize    := 0.U
    TLOEEndpoint.io.tlOpcode  := 0.U
    TLOEEndpoint.io.tlValid   := false.B
    TLOEEndpoint.io.tlMask    := 0.U
    TLOEEndpoint.io.tlSource  := 0.U
    TLOEEndpoint.io.tlParam   := 0.U

    TLOEEndpoint.io.incAccCreditValid := tilelinkHandler.io.incAccCreditValid
    TLOEEndpoint.io.incAccCreditChannel := tilelinkHandler.io.incAccCreditChannel
    TLOEEndpoint.io.incAccCreditAmount := tilelinkHandler.io.incAccCreditAmount

    tilelinkHandler.io.tlMsg              := TLOEEndpoint.io.tlMsg
    tilelinkHandler.io.tlMsgMask          := TLOEEndpoint.io.tlMsgMask
    tilelinkHandler.io.doTilelinkHandler  := TLOEEndpoint.io.doTilelinkHandler

    // Connect TLOEEndpoint to external ethernet interface
    io.txdata   := TLOEEndpoint.io.txdata
    io.txvalid  := TLOEEndpoint.io.txvalid
    io.txlast   := TLOEEndpoint.io.txlast
    io.txkeep   := TLOEEndpoint.io.txkeep

    // Connect external ethernet signals to TLOEEndpoint
    TLOEEndpoint.io.txready  := io.txready

    TLOEEndpoint.io.rxdata   := io.rxdata
    TLOEEndpoint.io.rxvalid  := io.rxvalid
    TLOEEndpoint.io.rxlast   := io.rxlast

    // VIO
    TLOEEndpoint.io.ox_open   := io.ox_open
    TLOEEndpoint.io.ox_close  := io.ox_close
    TLOEEndpoint.io.ox_debug1 := io.debug1
    TLOEEndpoint.io.ox_debug2 := io.debug2

    // MAC Address configuration (initialize to 0 for now)
    TLOEEndpoint.io.setSrcMac  := 0.U
    TLOEEndpoint.io.setDestMac := 0.U

    val debug_onInSource = RegInit(0.U(26.W))
    val debug_onInOpcode = RegInit(0.U(3.W))

    val debug_oxMACReg = RegInit(0.U(64.W))

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

       debug_onInSource := in.a.bits.source
       debug_onInOpcode := in.a.bits.opcode

      TLOEEndpoint.io.tlValid  := true.B // Mark the transmission as valid
    }

    // Mark the input channel 'a' as valid
    when (in.a.valid) {
        aValidReg := true.B
    }

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

    when (tilelinkHandler.io.ep_rxValid) {                 // RX valid signal received from Ethernet IP
      in.d.valid        := true.B                          // Mark the response as valid
      in.d.bits         := edge.AccessAck(in.a.bits)       // Generate an AccessAck response
      in.d.bits.opcode  := tilelinkHandler.io.ep_rxOpcode  // Set the opcode from the register
      in.d.bits.param   := tilelinkHandler.io.ep_rxParam   // Set the parameter from the register
      in.d.bits.size    := tilelinkHandler.io.ep_rxSize    // Set the size from the register
      in.d.bits.source  := tilelinkHandler.io.ep_rxSource  // Set the source ID from the register
      in.d.bits.sink    := 0.U                             // Set sink to 0
      in.d.bits.denied  := false.B                         // Mark as not denied

      // Optimized with Mux chain instead of switch
      when (tilelinkHandler.io.ep_rxOpcode === D_ACCESSACKDATA_OPCODE) {
        in.d.bits.data := MuxCase(tilelinkHandler.io.ep_rxData, Seq(
          (tilelinkHandler.io.ep_rxSize === 1.U) -> tilelinkHandler.io.ep_rxData(15, 0),
          (tilelinkHandler.io.ep_rxSize === 2.U) -> tilelinkHandler.io.ep_rxData(31, 0),
          (tilelinkHandler.io.ep_rxSize === 3.U) -> tilelinkHandler.io.ep_rxData(63, 0),
          (tilelinkHandler.io.ep_rxSize === 4.U) -> tilelinkHandler.io.ep_rxData(127, 0),
          (tilelinkHandler.io.ep_rxSize === 5.U) -> tilelinkHandler.io.ep_rxData(255, 0)
        ))
        in.d.bits.corrupt := false.B // Mark as not corrupt
      }.elsewhen (tilelinkHandler.io.ep_rxOpcode === D_ACCESSACK_OPCODE) {
        in.d.bits.data    := 0.U
      }
    }

    // Ready conditions for the input channel 'a' and response channel 'd'
    in.a.ready := in.a.valid || aValidReg
    in.d.ready := in.a.valid || aValidReg

    // IO ready signal is asserted when input is valid and opcode is Get or PutFullData
    io.ready := in.a.valid && (in.a.bits.opcode === A_GET_OPCODE || in.a.bits.opcode === A_PUTFULLDATA_OPCODE)
  }
}

trait OmniXtend { this: BaseSubsystem =>
  private val portName = "OmniXtend"
  implicit val p: Parameters

  val ox = LazyModule(new OmniXtendNode()(p))
  
  private val mbus = locateTLBusWrapper(MBUS)

  mbus.coupleTo(portName) { (ox.node
    :*= TLBuffer()
    :*= TLWidthWidget(mbus.beatBytes)
    :*= _)
  }
}

class WithOX(useAXI4: Boolean = false, useBlackBox: Boolean = false) extends Config((site, here, up) => {
  case OXKey => Some(OXParams(useAXI4 = useAXI4, useBlackBox = useBlackBox))
})
