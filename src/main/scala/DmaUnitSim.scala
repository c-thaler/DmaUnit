package dma_unit

import scala.util.Random
import scala.util.continuations._

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._

import dma_unit.axi._

case class AxiLite4Driver(axi : AxiLite4, clockDomain : ClockDomain) {

  def reset() : Unit = {
    axi.aw.valid #= false
    axi.w.valid #= false
    axi.ar.valid #= false
    axi.r.ready #= true
    axi.b.ready #= true
  }

  def write(address : BigInt, data : BigInt) : Unit = {
    axi.aw.payload.prot.assignBigInt(6)
    axi.w.payload.strb.assignBigInt(15)
    
    axi.aw.valid #= true
    axi.aw.payload.addr #= address

    axi.w.valid #= true
    axi.w.payload.data #= data

    clockDomain.waitSamplingWhere(axi.aw.ready.toBoolean && axi.w.ready.toBoolean)

    axi.aw.valid #= false
    axi.w.valid #= false
  }
}

//MyTopLevel's testbench
object DmaUnitSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new DmaUnit){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      val axiLite = AxiLite4Driver(dut.io.axi_slave, dut.clockDomain)
      val axiSim = AxiMemorySim(dut.io.axi_master, dut.clockDomain)

      axiSim.memory.loadBinary(0x4000l, "./test_data/test.txt")
      
      axiLite.reset()
      axiSim.start()

      dut.clockDomain.waitSampling(10)

      axiLite.write(0x0, 0x00004000l)
      axiLite.write(0x4, 35)
      axiLite.write(0x8, 0x00008000l)

      dut.clockDomain.waitSampling(100)
    }
  }
}
