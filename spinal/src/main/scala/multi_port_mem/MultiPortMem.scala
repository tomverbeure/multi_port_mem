/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package multi_port_mem

import spinal.core._
import spinal.lib._

import scala.util.Random

//Hardware definition
class MultiPortMem extends Component {
  val io = new Bundle {
    val cond0 = in  Bool
    val cond1 = in  Bool
    val flag  = out Bool
    val state = out UInt(8 bits)
  }
  val counter = Reg(UInt(8 bits)) init(0)

  when(io.cond0){
    counter := counter + 1
  }

  io.state := counter
  io.flag  := (counter === 0) | io.cond1
}

//Generate the MultiPortMem's Verilog
object MultiPortMemVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new MultiPortMem)
  }
}

//Generate the MultiPortMem's VHDL
object MultiPortMemVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new MultiPortMem)
  }
}


//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

//Generate the MultiPortMem's Verilog using the above custom configuration.
object MultiPortMemVerilogWithCustomConfig {
  def main(args: Array[String]) {
    MySpinalConfig.generateVerilog(new MultiPortMem)
  }
}
