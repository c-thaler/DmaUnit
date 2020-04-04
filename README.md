DmaUnit
=======

A simple AXI4 DMA unit with an AXI4-Lite register interface written in SpinalHDL.

Simulation
==========

Simple testbench is included. It also contains a nice AXI4 Bus Memory model.


Register map
============

* 0x00 - source address
* 0x04 - word count - 1
* 0x08 - destination address and trigger
