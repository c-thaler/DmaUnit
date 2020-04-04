package dma_unit.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

case class WriteJob(config : Axi4Config) extends Bundle {
    val address = UInt(config.addressWidth bits)
    val word_count = UInt(8 bits)
}

case class AxiWriter(config : Axi4Config) extends Component {
    val io = new Bundle {
        val axi_master = master(Axi4WriteOnly(config))
        val write_job = slave(Stream(WriteJob(config)))
        val write_in = slave(Stream(Bits(config.dataWidth bits)))
        val busy = out Bool
    }

    val fifo_depth = 32
    val write_fifo = StreamFifo(Bits(config.dataWidth bits), fifo_depth)
    
    
    write_fifo.io.push << io.write_in
    
    val fsm = new StateMachine {
        val aw_valid = Reg(Bool) init(False)
        val aw = Reg(Axi4Aw(config))
        val w_valid = Reg(Bool) init(False)
        val w = Reg(Axi4W(config))
        val words_left = Reg(UInt(8 bits))
        val len = Reg(UInt(8 bits))
        
        io.axi_master.writeRsp.ready := True
        io.axi_master.writeCmd.valid := aw_valid
        io.axi_master.writeCmd.payload := aw
        io.axi_master.writeData.valid := w_valid
        io.axi_master.writeData.payload.last := w.last
        io.axi_master.writeData.payload.strb := w.strb
        
        // Skip the output register for data since the FIFO already has an
        // output register.
        io.axi_master.writeData.payload.data := write_fifo.io.pop.payload
        
        aw.cache := "1111"
        aw.prot := "110"
        aw.setBurstINCR()
        
        w.strb.setAllTo(True)
        w.last := False
        
        io.write_job.ready := False
        write_fifo.io.pop.ready := False
        
        io.busy := True
        
        val idle : State = new State with EntryPoint {
            whenIsActive (
                io.busy := False,
                io.write_job.ready := True,
                when(io.write_job.valid) {
                    aw.addr := io.write_job.payload.address
                    words_left := io.write_job.payload.word_count
                    goto(wait_on_data)
                }
            )
        }

        val wait_on_data : State = new State {
            whenIsActive(
                when(write_fifo.io.occupancy >= 15 || write_fifo.io.occupancy >= words_left) {
                    aw_valid := True
                    when(write_fifo.io.occupancy >= 15) {
                        aw.len := 15
                        len := 15
                    }.otherwise {
                        aw.len := words_left
                        len := words_left
                    }
                    goto(start_aw)
                }
            )
        }

        val start_aw : State = new State {
            whenIsActive(
                when(io.axi_master.writeCmd.ready) {
                    aw_valid := False
                    when(len === 0) {
                        w.last := True
                    }
                    goto(write_bus)
                }
            )
        }

        val write_bus : State = new State {
            onEntry(
              w_valid := True,
            )
            whenIsActive(
                write_fifo.io.pop.ready := io.axi_master.writeData.ready,
                when(io.axi_master.writeData.ready) {
                    words_left := words_left - 1
                    len := len - 1
                    when(len === 1) {
                        w.last := True
                    }
                    when(words_left === 0) {
                        goto(idle)
                    }.elsewhen(len === 0) {
                        aw.addr := aw.addr + 16
                        goto(wait_on_data)
                    }
                }
            )
            onExit(
                w_valid := False
            )
        }
    }

    def writeJobFactory() : WriteJob = {
        WriteJob(config)
    }
}