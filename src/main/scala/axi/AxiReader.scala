package dma_unit.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.fsm._

case class ReadJob(config : Axi4Config, wordCountWidth : Int) extends Bundle {
    val address = UInt(config.addressWidth bits)
    val word_count = UInt(wordCountWidth bits)
}

case class AxiReader(config : Axi4Config, wordCountWidth : Int, maxBurstLength : Int, maxOutstandingReads : Int) extends Component {
    val io = new Bundle {
        val axi_master = master(Axi4ReadOnly(config))
        val read_job = slave(Stream(ReadJob(config, wordCountWidth)))
        val read_out = master(Stream(Bits(config.dataWidth bits)))
        val busy = out Bool
    }

    val fifo_depth = maxOutstandingReads * maxBurstLength
    val read_fifo = StreamFifo(Bits(config.dataWidth bits), fifo_depth)

    val outstandingWordsCounter = new Area {
        val incr = Bool
        val decr = Bool
        val incr_value = SInt(wordCountWidth bits)
        val outstandingWords = Reg(UInt(log2Up(fifo_depth + 1) bits)) init(0)
        val full = Bool
        var nextValue = SInt(wordCountWidth bits)

        full := (outstandingWords + maxBurstLength) >= fifo_depth

        nextValue := 0
        when(incr) {
            nextValue \= nextValue + incr_value
        }
        when(decr) {
            nextValue \= nextValue - 1
        }
        outstandingWords := U(S(outstandingWords) + nextValue).resized
    }

    io.axi_master.readRsp.ready := read_fifo.io.push.ready
    read_fifo.io.push.valid := io.axi_master.readRsp.valid
    read_fifo.io.push.payload := io.axi_master.readRsp.payload.data
    io.read_out << read_fifo.io.pop
    outstandingWordsCounter.decr := read_fifo.io.pop.valid && read_fifo.io.pop.ready

    val fsm_ar = new StateMachine {
        val words_left = Reg(UInt(wordCountWidth bits))
        val read_valid = Reg(Bool) init(False)
        val ar = Reg(Axi4Ar(config))

        io.axi_master.ar.valid := read_valid
        io.axi_master.ar.payload := ar

        ar.cache := "1111"
        ar.prot := "110"
        ar.setBurstINCR()

        outstandingWordsCounter.incr := False
        outstandingWordsCounter.incr_value := S(ar.len).resize(outstandingWordsCounter.incr_value.getWidth) + S(1)

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
                when(!outstandingWordsCounter.full) {
                    read_valid := True
                    when(words_left >= maxBurstLength - 1) {
                        ar.len := maxBurstLength - 1
                        words_left := words_left - maxBurstLength
                    }.otherwise {
                        ar.len := words_left.resized
                        words_left := 0
                    }
                    goto(wait_read)
                }
            )
        }

        val wait_read : State = new State {
            whenIsActive (
                when(io.axi_master.ar.ready) {
                    read_valid := False
                    outstandingWordsCounter.incr := True
                    goto(read_resp)
                }
            )
        }

        val read_resp : State = new State {
            whenIsActive (
                when(words_left =/= 0) {
                    ar.addr := ar.addr + fifo_depth
                    goto(start_read)
                }.otherwise {
                    goto(idle)
                }
            )
        }
    }

    def readJobFactory() : ReadJob = {
        ReadJob(config, wordCountWidth)
    }
}