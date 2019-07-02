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

class Mem_1w_1rs(config: MemConfig, readUnderWrite: ReadUnderWritePolicy = dontCare) extends Component
{
    val io = new Bundle {
        val wr_ena    = in(Bool)
        val wr_addr   = in(UInt(config.addrWidth bits))
        val wr_data   = in(Bits(config.dataWidth bits))

        val rd_ena    = in(Bool)
        val rd_addr   = in(UInt(config.addrWidth bits))
        val rd_data   = out(Bits(config.dataWidth bits))
    }

    val u_mem = Mem(Bits(config.dataWidth bits), wordCount = config.memorySize)
    u_mem.write(
        enable    = io.wr_ena,
        address   = io.wr_addr,
        data      = io.wr_data
    )

    val rd_data_mem = u_mem.readSync(
        enable    = io.rd_ena,
        address   = io.rd_addr
    )

    if (readUnderWrite == dontCare || readUnderWrite == readFirst)
        io.rd_data := rd_data_mem
    else {
        val rd_eq_wr = io.wr_addr === io.rd_addr

        val bypass_ena_p1 = RegNextWhen(io.wr_ena && rd_eq_wr, io.rd_ena)
        val wr_data_p1    = RegNextWhen(io.wr_data, io.wr_ena && io.rd_ena && rd_eq_wr)

        io.rd_data := bypass_ena_p1 ? wr_data_p1 | rd_data_mem
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
    val wr0_addr_p1 = RegNextWhen(io.wr0.addr, io.wr0.ena)
    val wr0_data_p1 = RegNextWhen(io.wr0.data, io.wr0.ena)

    val wr1_ena_p1  = RegNext(io.wr1.ena)
    val wr1_addr_p1 = RegNextWhen(io.wr1.addr, io.wr1.ena)
    val wr1_data_p1 = RegNextWhen(io.wr1.data, io.wr1.ena)

    val rd0_ena_p1  = RegNext(io.rd0.ena)
    val rd0_addr_p1 = RegNextWhen(io.rd0.addr, io.rd0.ena)

    val mem_bank0_w1_xor_data_p1 = Bits(config.dataWidth bits)
    val mem_bank1_w0_xor_data_p1 = Bits(config.dataWidth bits)

    val mem_bank0_r0_xor_data_p1 = Bits(config.dataWidth bits)
    val mem_bank1_r0_xor_data_p1 = Bits(config.dataWidth bits)

    //============================================================
    // Write Bank 0
    //============================================================

    val bank0_wr_xor_data_p1 = wr0_data_p1 ^ mem_bank1_w0_xor_data_p1

    // Write port RAM
    val u_mem_bank0_w1 = new Mem_1w_1rs(config, dontCare)
    u_mem_bank0_w1.io.wr_ena    <> wr0_ena_p1
    u_mem_bank0_w1.io.wr_addr   <> wr0_addr_p1
    u_mem_bank0_w1.io.wr_data   <> bank0_wr_xor_data_p1

    u_mem_bank0_w1.io.rd_ena    <> io.wr1.ena
    u_mem_bank0_w1.io.rd_addr   <> io.wr1.addr
    u_mem_bank0_w1.io.rd_data   <> mem_bank0_w1_xor_data_p1

    // Read port RAM
    val u_mem_bank0_r0 = new Mem_1w_1rs(config, writeFirst)
    u_mem_bank0_r0.io.wr_ena    <> wr0_ena_p1
    u_mem_bank0_r0.io.wr_addr   <> wr0_addr_p1
    u_mem_bank0_r0.io.wr_data   <> bank0_wr_xor_data_p1

    u_mem_bank0_r0.io.rd_ena    <> io.rd0.ena
    u_mem_bank0_r0.io.rd_addr   <> io.rd0.addr
    u_mem_bank0_r0.io.rd_data   <> mem_bank0_r0_xor_data_p1

    val bank0_rd0_raw_forward_p1 = wr0_ena_p1 && io.rd0.ena && wr0_addr_p1 === io.rd0.addr
    val bank0_rd0_xor_data_p1    = bank0_rd0_raw_forward_p1 ? bank0_wr_xor_data_p1 | mem_bank0_r0_xor_data_p1

    //============================================================
    // Write Bank 1
    //============================================================

    val bank1_wr_xor_data_p1 = wr1_data_p1 ^ mem_bank0_w1_xor_data_p1

    // Write port RAM
    val u_mem_bank1_w0 = new Mem_1w_1rs(config, dontCare)
    u_mem_bank1_w0.io.wr_ena    <> wr1_ena_p1
    u_mem_bank1_w0.io.wr_addr   <> wr1_addr_p1
    u_mem_bank1_w0.io.wr_data   <> bank1_wr_xor_data_p1

    u_mem_bank1_w0.io.rd_ena    <> io.wr0.ena
    u_mem_bank1_w0.io.rd_addr   <> io.wr0.addr
    u_mem_bank1_w0.io.rd_data   <> mem_bank1_w0_xor_data_p1

    // Read port RAM
    val u_mem_bank1_r0 = new Mem_1w_1rs(config, writeFirst)
    u_mem_bank1_r0.io.wr_ena    <> wr1_ena_p1
    u_mem_bank1_r0.io.wr_addr   <> wr1_addr_p1
    u_mem_bank1_r0.io.wr_data   <> bank1_wr_xor_data_p1

    u_mem_bank1_r0.io.rd_ena    <> io.rd0.ena
    u_mem_bank1_r0.io.rd_addr   <> io.rd0.addr
    u_mem_bank1_r0.io.rd_data   <> mem_bank1_r0_xor_data_p1

    val bank1_rd0_raw_forward_p1 = wr1_ena_p1 && io.rd0.ena && wr1_addr_p1 === io.rd0.addr
    val bank1_rd0_xor_data_p1    = bank1_rd0_raw_forward_p1 ? bank1_wr_xor_data_p1 | mem_bank1_r0_xor_data_p1

    //============================================================
    // Final output
    //============================================================


    val rd0_eq_wr0      = io.wr0.addr === io.rd0.addr
    val bypass0_ena_p1  = RegNextWhen(io.wr0.ena && rd0_eq_wr0, io.rd0.ena)
    val bypass0_data_p1 = RegNextWhen(io.wr0.data, io.wr0.ena && io.rd0.ena && rd0_eq_wr0)

    val rd0_eq_wr1      = io.wr1.addr === io.rd0.addr
    val bypass1_ena_p1  = RegNextWhen(io.wr1.ena && rd0_eq_wr1, io.rd0.ena)
    val bypass1_data_p1 = RegNextWhen(io.wr1.data, io.wr1.ena && io.rd0.ena && rd0_eq_wr1)

    io.rd0.data :=  bypass0_ena_p1 ? bypass0_data_p1 | 
                   (bypass1_ena_p1 ? bypass1_data_p1 | 
                                    bank0_rd0_xor_data_p1 ^ bank1_rd0_xor_data_p1) 
}

