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

case class MemConfig(
    memorySize      : Int,
    dataWidth       : Int
  )
{

    def addrWidth = log2Up(memorySize)
}

case class MemRd(config: MemConfig) extends Bundle with IMasterSlave
{
    val ena         = Bool
    val addr        = UInt(config.addrWidth bits)
    val data        = Bits(config.dataWidth bits)

    override def asMaster(): Unit = {
        out(ena, addr)
        in(data)
    }
}

case class MemWr(config: MemConfig) extends Bundle with IMasterSlave
{
    val ena         = Bool
    val addr        = UInt(config.addrWidth bits)
    val data        = Bits(config.dataWidth bits)

    override def asMaster(): Unit = {
        out(ena, addr, data)
    }
}


class MultiPortMem_1w_2rs(config: MemConfig) extends Component {
    val io = new Bundle {
        val wr0     = slave(MemWr(config))
        val rd0     = slave(MemRd(config))
        val rd1     = slave(MemRd(config))
    }

    // Clock wr transaction for forwarding (writeFirst mode: read gets the new write value)
    val wr0_ena_p1  = RegNext(io.wr0.ena)
    val wr0_addr_p1 = RegNext(io.wr0.addr)
    val wr0_data_p1 = RegNext(io.wr0.data)

    //============================================================
    // RD0 Port RAM
    //============================================================
    val rd0_ena_p1  = RegNext(io.rd0.ena)
    val rd0_addr_p1 = RegNext(io.rd0.addr)

    val u_mem0 = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize)
    u_mem0.write(
        enable    = io.wr0.ena,
        address   = io.wr0.addr,
        data      = io.wr0.data
    )

    val rd0_data_mem = u_mem0.readSync(
        enable    = io.rd0.ena,
        address   = io.rd0.addr
    )

    io.rd0.data := (wr0_ena_p1 && rd0_ena_p1 && wr0_addr_p1 === rd0_addr_p1) ? wr0_data_p1 | rd0_data_mem

    //============================================================
    // RD1 Port RAM
    //============================================================
    val rd1_ena_p1  = RegNext(io.rd1.ena)
    val rd1_addr_p1 = RegNext(io.rd1.addr)

    val u_mem1 = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize)
    u_mem1.write(
        enable    = io.wr0.ena,
        address   = io.wr0.addr,
        data      = io.wr0.data
    )

    val rd1_data_mem = u_mem1.readSync(
        enable    = io.rd1.ena,
        address   = io.rd1.addr
    )

    io.rd1.data := (wr0_ena_p1 && rd1_ena_p1 && wr0_addr_p1 === rd1_addr_p1) ? wr0_data_p1 | rd1_data_mem

}

class MultiPortMem_2w_1rs(config: MemConfig) extends Component {
    val io = new Bundle {
        val wr0     = slave(MemWr(config))
        val wr1     = slave(MemWr(config))
        val rd0     = slave(MemRd(config))
    }

    val wr0_ena_p1  = RegNext(io.wr0.ena)
    val wr0_addr_p1 = RegNext(io.wr0.addr)
    val wr0_data_p1 = RegNext(io.wr0.data)

    val wr1_ena_p1  = RegNext(io.wr1.ena)
    val wr1_addr_p1 = RegNext(io.wr1.addr)
    val wr1_data_p1 = RegNext(io.wr1.data)

    val rd0_ena_p1  = RegNext(io.rd0.ena)
    val rd0_addr_p1 = RegNext(io.rd0.addr)

    val mem_bank1_w0_xor_data_p1 = Bits(config.dataWidth bits)
    val mem_bank0_w1_xor_data_p1 = Bits(config.dataWidth bits)

    //============================================================
    // Write Bank 0
    //============================================================

    val bank0_wr_xor_data_p1 = wr0_data_p1 ^ mem_bank1_w0_xor_data_p1

    // Write port RAM
    val u_mem_bank0_w1 = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize)
    u_mem_bank0_w1.write(
        enable    = wr0_ena_p1,
        address   = wr0_addr_p1,
        data      = bank0_wr_xor_data_p1
    )
    mem_bank0_w1_xor_data_p1 := u_mem_bank0_w1.readSync(
        enable    = io.wr1.ena,
        address   = io.wr1.addr
    )

    // Read port RAM
    val u_mem_bank0_r0 = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize)
    u_mem_bank0_r0.write(
        enable    = wr0_ena_p1,
        address   = wr0_addr_p1,
        data      = bank0_wr_xor_data_p1
    )
    val mem_bank0_rd0_xor_data_p1 = u_mem_bank0_r0.readSync(
        enable    = io.rd0.ena,
        address   = io.rd0.addr
    )

    val bank0_rd0_xor_data_p1 = (wr0_ena_p1 && rd0_ena_p1 && wr0_addr_p1 === rd0_addr_p1) ? bank0_wr_xor_data_p1 | mem_bank0_rd0_xor_data_p1

    //============================================================
    // Write Bank 1
    //============================================================

    val bank1_wr_xor_data_p1 = wr1_data_p1 ^ mem_bank0_w1_xor_data_p1

    // Write port RAM
    val u_mem_bank1_w0 = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize)
    u_mem_bank1_w0.write(
        enable    = wr1_ena_p1,
        address   = wr1_addr_p1,
        data      = bank1_wr_xor_data_p1
    )
    mem_bank1_w0_xor_data_p1 := u_mem_bank1_w0.readSync(
        enable    = io.wr0.ena,
        address   = io.wr0.addr
    )

    // Read port RAM
    val u_mem_bank1_r0 = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize)
    u_mem_bank1_r0.write(
        enable    = wr1_ena_p1,
        address   = wr1_addr_p1,
        data      = bank1_wr_xor_data_p1
    )
    val mem_bank1_rd0_xor_data_p1 = u_mem_bank1_r0.readSync(
        enable    = io.rd0.ena,
        address   = io.rd0.addr
    )

    val bank1_rd0_xor_data_p1 = (wr1_ena_p1 && rd0_ena_p1 && wr1_addr_p1 === rd0_addr_p1) ? bank1_wr_xor_data_p1 | mem_bank1_rd0_xor_data_p1

    io.rd0.data := bank0_rd0_xor_data_p1 ^ bank1_rd0_xor_data_p1
}

