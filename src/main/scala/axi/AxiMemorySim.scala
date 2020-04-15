package dma_unit.axi

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import java.nio.file.Paths
import java.nio.file.Files

class MemoryPage(size : Int) {
    val data = new Array[Byte](size)

    def clear(value : Byte) : Unit = {
      data.transform(x => value)
    }

    def read(offset : Int) : Byte = {
        this.data(offset)
    }

    def write(offset : Int, data : Byte) : Unit = {
        this.data(offset) = data
    }
    
    /** Reads an array from this page.
     * 
     * @param offset Offset into page
     * @return Byte array containing the read bytes. Reads may be limited by the page end.
     */
    def readArray(offset : Int, len : Int) : Array[Byte] = {
        var length = scala.math.min(len, size - offset)
        var data = new Array[Byte](length)

        for(i <- 0 until length) {
          data(i) = this.data(offset + i)
        }

        data
    }

    /** Writes an array to this page.
     * 
     * @param offset Offset into page.
     * @param data The byte array.
     * @return Number of bytes written. Writes may be limited by the page end.
     */
    def writeArray(offset : Int, data : Array[Byte]) : Int = {
        var length = scala.math.min(data.length, size - offset)

        for(i <- 0 until length) {
          this.data(offset + i) = data(i)
        }

        length
    }
}

case class SparseMemory() {
  val memory = Array.fill[MemoryPage](4096)(null)

  def allocPage() : MemoryPage = {
    val page = new MemoryPage(1024*1024)
    page.clear(0xcd.toByte)
    page
  }

  def invalidPage() : MemoryPage = {
    val page = new MemoryPage(1024*1024)
    page.clear(0xcd.toByte)
    page
  }

  def getElseAllocPage(address : Long) : MemoryPage = {
    var index = (address >> 20).toInt
    if(memory(index) == null) memory(index) = allocPage()
    memory(index)
  }

  def getElseInvalidPage(address : Long) : MemoryPage = {
    var index = (address >> 20).toInt
    if(memory(index) == null) {
      println("Page fault: " + address.toHexString)
      invalidPage()
    }
    else
      memory(index)
  }

  def getPageIndex(address : Long) : Int = {
    (address >> 20).toInt
  }

  def getOffset(address : Long) : Int = {
    val mask = (1 << 20) - 1
    (address & mask).toInt
  }

  def read(address : Long) : Byte = {
    getElseAllocPage(address).read(getOffset(address))
  }

  def write(address : Long, data : Byte) : Unit = {
    getElseAllocPage(address).write(getOffset(address), data)
  }

  def readArray(address : Long, len : Long) : Array[Byte] = {
    val startPageIndex = getPageIndex(address)
    val endPageIndex = getPageIndex(address + len - 1)
    var offset = getOffset(address)
    val buffer = new mutable.ArrayBuffer[Byte](0)
    
    for(i <- startPageIndex to endPageIndex) {
      val page = getElseInvalidPage(i)
      val readArray = page.readArray(offset, len.toInt - buffer.length)
      buffer.appendAll(readArray)
      offset = 0
    }

    buffer.toArray
  }

  def writeArray(address : Long, data : Array[Byte]) : Unit = {
    val startPageIndex = getPageIndex(address)
    val endPageIndex = getPageIndex(address + data.length - 1)
    var offset = getOffset(address)
    
    for(i <- startPageIndex to endPageIndex) {
      val page = getElseAllocPage(i)
      val bytesWritten = page.writeArray(offset, data)
      data.drop(bytesWritten)
      offset = 0
    }
  }

  /** Reads a BigInt value from the given address.
   * 
   * @param address Read address.
   * @param width Length of the byte array to be read in bytes.
   * @return BigInt read from the given address.
   */
  def readBigInt(address : Long, length : Int) : BigInt = {
    val dataArray = readArray(address, length)
    val buffer = dataArray.reverse.toBuffer // revert for Little Endian representation

    // We never want negative numbers
    buffer.prepend(0.toByte)

    BigInt(buffer.toArray)
  }

  /** Writes a BigInt value to the given address.
   * The BigInt will be resized to a byte Array of given width.
   * The data will be trimmed if it is bigger than the given width.
   * If it is smaller, the unused bytes will be filled with '0x00'.
   * 
   * @param address Write address.
   * @param data Data to be written.
   * @param width Width of the byte Array the data is resized to (if necessary).
   */
  def writeBigInt(address : Long, data : BigInt, width : Int) {
    var dataArray = data.toByteArray.reverse
    var length = scala.math.min(width, dataArray.length)
    var result = Array.fill[Byte](width)(0.toByte)

    for(i <- 0 until length)
      result(i) = dataArray(i)
    
    writeArray(address, result)
  }

  def loadBinary(address : Long, file : String): Unit = {
    val byteArray = Files.readAllBytes(Paths.get(file))
    writeArray(address, byteArray)
  }
}

case class AxiJob (
  address     : Long,
  burstLength : Int
) {
  // check for read/write over 4k boundary
}

case class AxiMemorySim(axi : Axi4, clockDomain : ClockDomain) {
  val memory = SparseMemory()
  val pending_reads = new mutable.Queue[AxiJob]
  val pending_writes = new mutable.Queue[AxiJob]

  /** Bus word width in bytes */
  val busWordWidth = axi.config.dataWidth / 8

  def newAxiJob(address : Long, burstLength : Int) : AxiJob = {
    AxiJob(address, burstLength)
  }

  def start() : Unit = {
    fork {
      handleAr(axi.ar)
    }

    fork {
      handleR(axi.r)
    }

    fork {
      handleAw(axi.aw)
    }

    fork {
      handleW(axi.w, axi.b)
    }
  }

  def handleAr(ar : Stream[Axi4Ar]) : Unit = {
    println("Handling AXI4 Master read cmds...")

    ar.ready #= false

    while(true) {
      ar.ready #= true
      clockDomain.waitSamplingWhere(ar.valid.toBoolean)
      ar.ready #= false
      
      pending_reads += newAxiJob(ar.payload.addr.toLong, ar.payload.len.toInt)

      println("AXI4 read cmd: addr=0x" + ar.payload.addr.toLong.toHexString + " count=" + (ar.payload.len.toBigInt+1))
    }
  }

  def handleR(r : Stream[Axi4R]) : Unit = {
    println("Handling AXI4 Master read resp...")

    r.valid #= false
    r.payload.last #= false

    while(true) {
      clockDomain.waitSampling(10)

      if(pending_reads.nonEmpty) {
        var job = pending_reads.dequeue()
       
        r.valid #= true

        for(i <- 0 to job.burstLength) {
          if(i == job.burstLength)
            r.payload.last #= true
          r.payload.data #= memory.readBigInt(job.address + i * busWordWidth, busWordWidth)
          clockDomain.waitSampling()
        }

        r.valid #= false
        r.payload.last #= false

        println("AXI4 read rsp: addr=0x" + job.address.toLong.toHexString + " count=" + (job.burstLength+1))
      }
    }
  }

  def handleAw(aw : Stream[Axi4Aw]) : Unit = {
    println("Handling AXI4 Master write cmds...")

    aw.ready #= false

    while(true) {
      aw.ready #= true
      clockDomain.waitSamplingWhere(aw.valid.toBoolean)
      aw.ready #= false

      pending_writes += newAxiJob(aw.payload.addr.toLong, aw.payload.len.toInt)

      println("AXI4 write cmd: addr=0x" + aw.payload.addr.toLong.toHexString + " count=" + (aw.payload.len.toBigInt+1))
    }
  }

  def handleW(w : Stream[Axi4W], b : Stream[Axi4B]) : Unit = {
    println("Handling AXI4 Master write...")

    w.ready #= false
    b.valid #= false

    while(true) {
      clockDomain.waitSampling(10)

      assert(!(pending_writes.isEmpty && w.valid.toBoolean), "AXI: write without write cmd")

      if(pending_writes.nonEmpty) {
        var job = pending_writes.dequeue()
        var count = job.burstLength
       
        w.ready #= true

        for(i <- 0 to job.burstLength) {
          clockDomain.waitSamplingWhere(w.valid.toBoolean)
          memory.writeBigInt(job.address + i * busWordWidth, w.payload.data.toBigInt, busWordWidth)
        }

        w.ready #= false

        b.valid #= true
        b.payload.resp #= 0
        clockDomain.waitSamplingWhere(b.ready.toBoolean)
        b.valid #= false

        println("AXI4 write: addr=0x" + job.address.toLong.toHexString + " count=" + (job.burstLength+1))
      }
    }
  }
}