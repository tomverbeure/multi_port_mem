package multi_port_mem

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random


//MultiPortMem's testbench
object MultiPortMemSim {
    def main(args: Array[String]) {

        var stimuli = Array[ ( (Boolean, Long, Long), (Boolean, Long, Long), (Boolean, Long, Long) )](
              // WR0                  WR1                  RD0
              // Individual read/writes. Dummy cycle in between
              ( (true,  0x10, 0xAA), (false, 0x00, 0x00), (false, 0x00, 0x00) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (false, 0x00, 0x00) ),
              ( (false, 0x00, 0x00), (true,  0x20, 0x55), (false, 0x00, 0x00) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (false, 0x00, 0x00) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (true,  0x10, 0xAA) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (false, 0x00, 0x00) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (true,  0x20, 0x55) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (false, 0x00, 0x00) ),

              // Write immediately followed by read
              ( (true,  0x10, 0x44), (false, 0x00, 0x00), (false, 0x00, 0x00) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (true,  0x10, 0x44) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (false, 0x00, 0x00) ),

              // Coincident writes, dummy cycle before read
              ( (true,  0x10, 0x11), (true,  0x20, 0x22), (false, 0x00, 0x00) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (false, 0x00, 0x00) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (true,  0x10, 0x11) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (true,  0x20, 0x22) ),
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (false, 0x00, 0x00) ),

              //
              ( (false, 0x00, 0x00), (false, 0x00, 0x00), (false, 0x00, 0x00) )
            )

        SimConfig.withWave.doSim(new MultiPortMemTop){dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.io.wr0.ena      #= false
            dut.io.wr1.ena      #= false
            dut.io.rd0.ena      #= false

            dut.clockDomain.waitRisingEdge()

            for(addr <- 0 to 100){
                dut.io.wr0.ena      #= true
                dut.io.wr0.addr     #= addr
                dut.io.wr0.data     #= 0

                dut.io.wr1.ena      #= false

                dut.clockDomain.waitRisingEdge()
            }

            for(addr <- 0 to 100){
                dut.io.wr0.ena      #= false

                dut.io.wr1.ena      #= true
                dut.io.wr1.addr     #= addr
                dut.io.wr1.data     #= 0

                dut.clockDomain.waitRisingEdge()
            }

            var test_rd       = false
            var test_rd_addr  = 0L
            var test_rd_data  = 0L

            for(stim <- stimuli){
                dut.io.wr0.ena      #= stim._1._1
                dut.io.wr0.addr     #= stim._1._2
                dut.io.wr0.data     #= stim._1._3

                dut.io.wr1.ena      #= stim._2._1
                dut.io.wr1.addr     #= stim._2._2
                dut.io.wr1.data     #= stim._2._3

                dut.io.rd0.ena      #= stim._3._1
                dut.io.rd0.addr     #= stim._3._2

                dut.clockDomain.waitRisingEdge()

                if (test_rd){
                    printf("RdAddr: %08x -> Exp: %08x, Act: %08x\n", test_rd_addr, test_rd_data, dut.io.rd0.data.toLong);
                    assert(dut.io.rd0.data.toLong == test_rd_data)
                }

                test_rd      = stim._3._1
                test_rd_addr = stim._3._2
                test_rd_data = stim._3._3

            }

            dut.io.wr0.ena      #= false
            dut.io.wr1.ena      #= false
            dut.io.rd0.ena      #= false

            for(wait <- 0 to 10){
                dut.clockDomain.waitRisingEdge()
            }

        }
    }
}
