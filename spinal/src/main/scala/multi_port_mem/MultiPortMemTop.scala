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

class MultiPortMemTop extends Component {

    val memConfig = MemConfig(memorySize = 1024, dataWidth = 32)

    val io = new Bundle {
        val wr0     = slave(MemWr(memConfig))
        val rd0     = slave(MemRd(memConfig))
        val rd1     = slave(MemRd(memConfig))
    }

    val u_mem = new MultiPortMem_1w_2rs(memConfig)
    u_mem.io.wr0      <> io.wr0
    u_mem.io.rd0      <> io.rd0
    u_mem.io.rd1      <> io.rd1

}

object MultiPortMemTopVerilog {
    def main(args: Array[String]) {
        SpinalVerilog(new MultiPortMemTop)
    }
}

/*
//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

//Generate the MultiPortMem's Verilog using the above custom configuration.
object MultiPortMemVerilogWithCustomConfig {
  def main(args: Array[String]) {
    MySpinalConfig.generateVerilog(new MultiPortMem)
  }
}
*/
