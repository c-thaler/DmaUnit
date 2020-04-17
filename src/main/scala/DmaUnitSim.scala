package dma_unit

import scala.util.Random

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._

import dma_unit.axi._
import spinal.lib.eda.altera.QSysify

object simConfig{
  def getAxiMemorySimConfig = AxiMemorySimConfig(
    maxOutstandingReads  = 8,
    maxOutstandingWrites = 8
  )
}

//MyTopLevel's testbench
object DmaUnitSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new DmaUnit){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      val axiLite = AxiLite4Driver(dut.io.axi_slave, dut.clockDomain)
      val axiSim = AxiMemorySim(dut.io.axi_master, dut.clockDomain, simConfig.getAxiMemorySimConfig)

      axiSim.memory.loadBinary(0x4000l, "./test_data/test.txt")
      
      axiLite.reset()
      axiSim.start()

      dut.clockDomain.waitSampling(10)

      axiLite.read(0x0)
      axiLite.write(0x4, 0x00004000l)
      axiLite.write(0x8, 300)
      axiLite.write(0xc, 0x00008000l)

      dut.clockDomain.waitSampling(1000)
    }
  }
}
