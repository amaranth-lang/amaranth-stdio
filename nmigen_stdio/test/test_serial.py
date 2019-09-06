import unittest
from nmigen import *
from nmigen.lib.fifo import SyncFIFO
from nmigen.back.pysim import *

from ..serial import *


def simulation_test(dut, process):
    with Simulator(dut, vcd_file=open("test.vcd", "w")) as sim:
        sim.add_clock(1e-6)
        sim.add_sync_process(process)
        sim.run()


class AsyncSerialRXTestCase(unittest.TestCase):
    def tx_period(self):
        for _ in range((yield self.dut.divisor) + 1):
            yield

    def tx_bits(self, bits):
        for bit in bits:
            yield from self.tx_period()
            yield self.dut.i.eq(bit)

    def rx_test(self, bits, *, data=None, errors=None):
        def process():
            self.assertFalse((yield self.dut.rdy))
            yield self.dut.ack.eq(1)
            yield from self.tx_bits(bits)
            while not (yield self.dut.rdy):
                yield
            if data is not None:
                self.assertFalse((yield self.dut.err))
                self.assertEqual((yield self.dut.data), data)
            if errors is not None:
                self.assertTrue((yield self.dut.err))
                for error in errors:
                    self.assertTrue((yield getattr(self.dut.err, error)))
        simulation_test(self.dut, process)

    def test_8n1(self):
        self.dut = AsyncSerialRX(divisor=7, data_bits=8, parity="none")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1], data=0b10101110)

    def test_16n1(self):
        self.dut = AsyncSerialRX(divisor=7, data_bits=16, parity="none")
        self.rx_test([0, 1,0,1,0,1,1,1,0,1,1,1,1,0,0,0,0, 1],
                     data=0b1010111011110000)

    def test_8m1(self):
        self.dut = AsyncSerialRX(divisor=7, data_bits=8, parity="mark")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1, 1], data=0b10101110)
        self.rx_test([0, 1,0,1,0,1,1,0,0, 1, 1], data=0b10101100)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 0, 1], errors={"parity"})

    def test_8s1(self):
        self.dut = AsyncSerialRX(divisor=7, data_bits=8, parity="space")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 0, 1], data=0b10101110)
        self.rx_test([0, 1,0,1,0,1,1,0,0, 0, 1], data=0b10101100)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1, 1], errors={"parity"})

    def test_8e1(self):
        self.dut = AsyncSerialRX(divisor=7, data_bits=8, parity="even")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1, 1], data=0b10101110)
        self.rx_test([0, 1,0,1,0,1,1,0,0, 0, 1], data=0b10101100)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 0, 1], errors={"parity"})

    def test_8o1(self):
        self.dut = AsyncSerialRX(divisor=7, data_bits=8, parity="odd")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 0, 1], data=0b10101110)
        self.rx_test([0, 1,0,1,0,1,1,0,0, 1, 1], data=0b10101100)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1, 1], errors={"parity"})

    def test_err_frame(self):
        self.dut = AsyncSerialRX(divisor=7)
        self.rx_test([0, 0,0,0,0,0,0,0,0, 0], errors={"frame"})

    def test_err_overflow(self):
        self.dut = AsyncSerialRX(divisor=7)
        def process():
            self.assertFalse((yield self.dut.rdy))
            yield from self.tx_bits([0, 0,0,0,0,0,0,0,0, 1])
            yield from self.tx_period()
            self.assertFalse((yield self.dut.rdy))
            self.assertTrue((yield self.dut.err.overflow))
        simulation_test(self.dut, process)

    def test_fifo(self):
        self.dut  = AsyncSerialRX(divisor=7)
        self.fifo = SyncFIFO(width=8, depth=4)
        m = Module()
        m.submodules.rx   = self.dut
        m.submodules.fifo = self.fifo
        m.d.comb += [
            self.dut.ack.eq(self.fifo.w_rdy),
            self.fifo.w_en.eq(self.dut.rdy),
            self.fifo.w_data.eq(self.dut.data),
        ]
        def process():
            yield from self.tx_bits([0, 1,0,1,0,1,0,1,0, 1,
                                     0, 0,1,0,1,0,1,0,1, 1])
            yield from self.tx_period()
            self.assertEqual((yield from self.fifo.read()), 0xAA)
            self.assertEqual((yield from self.fifo.read()), 0x55)
            yield
            self.assertFalse((yield self.fifo.r_rdy))
        simulation_test(m, process)
