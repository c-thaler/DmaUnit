package dma_unit.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

case class ReadJob(config : Axi4Config) extends Bundle {
    val address = UInt(config.addressWidth bits)
    val word_count = UInt(8 bits)
}

case class AxiReader(config : Axi4Config) extends Component {
    val io = new Bundle {
        val axi_master = master(Axi4ReadOnly(config))
        val read_job = slave(Stream(ReadJob(config)))
        val read_out = master(Stream(Bits(config.dataWidth bits)))
        val busy = out Bool
    }

    val fifo_depth = 32
    val read_fifo = StreamFifo(Bits(config.dataWidth bits), fifo_depth)

    io.axi_master.readRsp.ready := read_fifo.io.push.ready
    read_fifo.io.push.valid := io.axi_master.readRsp.valid
    read_fifo.io.push.payload := io.axi_master.readRsp.payload.data
    io.read_out << read_fifo.io.pop

    val fsm = new StateMachine {
        val words_left = Reg(UInt(8 bits))
        val read_valid = Reg(Bool) init(False)
        val ar = Reg(Axi4Ar(config))

        io.axi_master.ar.valid := read_valid
        io.axi_master.ar.payload := ar

        ar.cache := "1111"
        ar.prot := "110"
        ar.setBurstINCR()

        io.read_job.ready := False

        io.busy := True

        val idle : State = new State with EntryPoint {
            whenIsActive (
                io.busy := False,
                io.read_job.ready := True,
                when(io.read_job.valid) {
                    ar.addr := io.read_job.payload.address
                    words_left := io.read_job.payload.word_count
                    goto(start_read)
                }
            )
        }

        val start_read : State = new State {
            whenIsActive(
                when(read_fifo.io.availability >= 16) {
                    read_valid := True
                    goto(wait_read)
                },
                when(words_left >= 15) {
                    ar.len := 15
                    words_left := words_left - 16
                }.otherwise {
                    ar.len := words_left.resized
                    words_left := 0
                }
            )
        }

        val wait_read : State = new State {
            whenIsActive (
                when(io.axi_master.ar.ready) {
                    read_valid := False
                    goto(read_resp)
                }
            )
        }

        val read_resp : State = new State {
            whenIsActive (
                when(io.axi_master.r.last) {
                    when(words_left =/= 0) {
                        ar.addr := ar.addr + 16
                        goto(start_read)
                    }.otherwise {
                        goto(idle)
                    }
                }
            )
        }
    }

    def readJobFactory() : ReadJob = {
        ReadJob(config)
    }
}