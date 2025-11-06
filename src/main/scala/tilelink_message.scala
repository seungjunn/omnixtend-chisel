package omnixtend

import chisel3._
import chisel3.util._

import OmniXtendConstants._

object TlMsgFlits {
  // Helper function to convert size to flits
  def convertSizeToFlits(size: UInt): UInt = {
    (size + 7.U) / 8.U  // (size + 7) / 8
  }

  // Calculate header size in bytes
  def getHeaderSize(chan: UInt, opcode: UInt): UInt = {
    val headerSize = Wire(UInt(4.W))
    headerSize := 0.U  // Default value
    
    switch(chan) {
      is(CHANNEL_A) {
        headerSize := 4.U  // 2^4 : 16 bytes
      }
      is(CHANNEL_B) {
        headerSize := 4.U  // 2^4 : 16 bytes
      }
      is(CHANNEL_C) {
        headerSize := 4.U  // 2^4 : 16 bytes
      }
      is(CHANNEL_D) {
        when(opcode === D_ACCESSACK_OPCODE || opcode === D_HINTACK_OPCODE || 
             opcode === D_RELEASEACK_OPCODE || opcode === D_ACCESSACKDATA_OPCODE) {
          headerSize := 3.U  // 2^3 : 8 bytes
        }.elsewhen(opcode === D_GRANT_OPCODE || opcode === D_GRANTDATA_OPCODE) {
          headerSize := 4.U  // 2^4 : 16 bytes
        }
      }
      is(CHANNEL_E) {
        headerSize := 3.U  // 2^3 : 8 bytes
      }
    }
    
    (1.U << headerSize)
  }

  // Calculate data size in bytes
  def getDataSize(chan: UInt, opcode: UInt, size: UInt): UInt = {
    val dataSize = Wire(UInt(4.W))
    dataSize := 0.U  // Default value
    
    switch(chan) {
      is(CHANNEL_A) {
        when(opcode === A_PUTFULLDATA_OPCODE || opcode === A_ARITHMETICDATA_OPCODE || 
             opcode === A_LOGICALDATA_OPCODE || opcode === A_PUTPARTIALDATA_OPCODE) {
          dataSize := size
        }
      }
      is(CHANNEL_B) {
        when(opcode === B_PUTFULLDATA_OPCODE || opcode === B_ARITHMETICDATA_OPCODE || 
             opcode === B_LOGICALDATA_OPCODE || opcode === B_PUTPARTIALDATA_OPCODE) {
          dataSize := size
        }
      }
      is(CHANNEL_C) {
        when(opcode === C_ACCESSACKDATA_OPCODE || opcode === C_PROBEACKDATA_OPCODE || 
             opcode === C_RELEASEDATA_OPCODE) {
          dataSize := size
        }
      }
      is(CHANNEL_D) {
        when(opcode === D_ACCESSACKDATA_OPCODE || opcode === D_GRANTDATA_OPCODE) {
          dataSize := size
        }
      }
    }
    
    Mux(dataSize === 0.U, 0.U, 1.U << dataSize)
  }

  // Calculate total flits for a TileLink message
  def getFlitsCnt(chan: UInt, opcode: UInt, size: UInt): UInt = {
    val headerSize = getHeaderSize(chan, opcode)
    val dataSize = getDataSize(chan, opcode, size)
    
    val headerFlits = Wire(UInt(8.W))
    val dataFlits = Wire(UInt(8.W))
    
    headerFlits := 0.U
    dataFlits := 0.U
    
    when(headerSize >= 0.U) {
      headerFlits := convertSizeToFlits(headerSize)
    }
    when(dataSize >= 0.U) {
      dataFlits := convertSizeToFlits(dataSize)
    }
 
    headerFlits + dataFlits
  }
}