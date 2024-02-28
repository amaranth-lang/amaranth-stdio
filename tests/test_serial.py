# amaranth: UnusedElaboratable=no

from unittest import TestCase

from amaranth import *
from amaranth.lib.data import StructLayout
from amaranth.lib.fifo import SyncFIFO
from amaranth.lib.wiring import *
from amaranth.sim import *

from amaranth_stdio.serial import *


def simulation_test(dut, process):
    sim = Simulator(dut)
    with sim.write_vcd("test.vcd"):
        sim.add_clock(1e-6)
        sim.add_sync_process(process)
        sim.run()


class _DummyPins:
    def __init__(self):
        self.rx = Signal(StructLayout({"i": 1}), reset={"i": 1})
        self.tx = Signal(StructLayout({"o": 1}), reset={"o": 1})


class AsyncSerialRXSignatureTestCase(TestCase):
    def test_simple(self):
        sig = AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 8)
        self.assertEqual(sig.data_bits, 7)
        self.assertEqual(sig.parity, Parity.EVEN)
        self.assertEqual(sig.members, Signature({
            "divisor": In(unsigned(8), reset=10),
            "data":    Out(unsigned(7)),
            "err":     Out(StructLayout({"overflow": 1, "frame": 1, "parity": 1})),
            "rdy":     Out(unsigned(1)),
            "ack":     In(unsigned(1)),
            "i":       In(unsigned(1), reset=1),
        }).members)

    def test_defaults(self):
        sig = AsyncSerialRX.Signature(divisor=10)
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 4)
        self.assertEqual(sig.data_bits, 8)
        self.assertEqual(sig.parity, Parity.NONE)

    def test_eq(self):
        self.assertEqual(AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                         AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"))
        # different divisor
        self.assertNotEqual(AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerialRX.Signature(divisor= 8, divisor_bits=8, data_bits=7, parity="even"))
        # different divisor_bits
        self.assertNotEqual(AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerialRX.Signature(divisor=10, divisor_bits=4, data_bits=7, parity="even"))
        # different data_bits
        self.assertNotEqual(AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="even"))
        # different parity
        self.assertNotEqual(AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="even"),
                            AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="none"))

    def test_repr(self):
        sig = AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(repr(sig), "AsyncSerialRX.Signature(SignatureMembers({"
                                        "'divisor': In(unsigned(8), reset=10), "
                                        "'data': Out(unsigned(7)), "
                                        "'err': Out(StructLayout({"
                                            "'overflow': 1, "
                                            "'frame': 1, "
                                            "'parity': 1})), "
                                        "'rdy': Out(unsigned(1)), "
                                        "'ack': In(unsigned(1)), "
                                        "'i': In(unsigned(1), reset=1)}))")

    def test_wrong_divisor(self):
        with self.assertRaisesRegex(TypeError,
                r"Divisor initial value must be an integer greater than or equal to 5, not 4"):
            AsyncSerialRX.Signature(divisor=4)
        with self.assertRaisesRegex(TypeError,
                r"Divisor initial value must be an integer greater than or equal to 5, not 4"):
            AsyncSerialRX.Signature.check_parameters(divisor=4)

    def test_wrong_divisor_bits(self):
        with self.assertRaisesRegex(TypeError,
                r"Divisor bits must be an integer greater than or equal to 4, not 3"):
            AsyncSerialRX.Signature(divisor=8, divisor_bits=3)
        with self.assertRaisesRegex(TypeError,
                r"Divisor bits must be an integer greater than or equal to 4, not 3"):
            AsyncSerialRX.Signature.check_parameters(divisor=8, divisor_bits=3)

    def test_wrong_data_bits(self):
        with self.assertRaisesRegex(TypeError,
                r"Data bits must be a non-negative integer, not -1"):
            AsyncSerialRX.Signature(divisor=10, data_bits=-1)
        with self.assertRaisesRegex(TypeError,
                r"Data bits must be a non-negative integer, not -1"):
            AsyncSerialRX.Signature.check_parameters(divisor=10, data_bits=-1)

    def test_wrong_parity(self):
        with self.assertRaisesRegex(ValueError, r"'foo' is not a valid Parity"):
            AsyncSerialRX.Signature(divisor=10, parity="foo")
        with self.assertRaisesRegex(ValueError, r"'foo' is not a valid Parity"):
            AsyncSerialRX.Signature.check_parameters(divisor=10, parity="foo")


class AsyncSerialRXTestCase(TestCase):
    def tx_period(self):
        for _ in range((yield self.dut.divisor)):
            yield

    def tx_bits(self, bits, pins=None):
        if pins is not None:
            rx_i = pins.rx.i
        else:
            rx_i = self.dut.i
        for bit in bits:
            yield from self.tx_period()
            yield rx_i.eq(bit)

    def rx_test(self, bits, *, data=None, errors=None, pins=None):
        def process():
            self.assertFalse((yield self.dut.rdy))
            yield self.dut.ack.eq(1)
            yield from self.tx_bits(bits, pins)
            while not (yield self.dut.rdy):
                yield
            if data is not None:
                self.assertFalse((yield self.dut.err.overflow))
                self.assertFalse((yield self.dut.err.frame))
                self.assertFalse((yield self.dut.err.parity))
                self.assertEqual((yield self.dut.data), data)
            if errors is not None:
                for error in errors:
                    self.assertTrue((yield getattr(self.dut.err, error)))
        simulation_test(self.dut, process)

    def test_signature(self):
        dut = AsyncSerialRX(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertIsInstance(dut.signature, AsyncSerialRX.Signature)
        self.assertEqual(dut.signature.divisor, 10)
        self.assertEqual(dut.signature.divisor_bits, 8)
        self.assertEqual(dut.signature.data_bits, 7)
        self.assertEqual(dut.signature.parity, Parity.EVEN)

    def test_defaults(self):
        dut = AsyncSerialRX(divisor=10)
        self.assertEqual(dut.signature.divisor, 10)
        self.assertEqual(dut.signature.divisor_bits, 4)
        self.assertEqual(dut.signature.data_bits, 8)
        self.assertEqual(dut.signature.parity, Parity.NONE)

    def test_8n1(self):
        self.dut = AsyncSerialRX(divisor=5, data_bits=8, parity="none")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1], data=0b01110101)

    def test_16n1(self):
        self.dut = AsyncSerialRX(divisor=5, data_bits=16, parity="none")
        self.rx_test([0, 1,0,1,0,1,1,1,0,1,1,1,1,0,0,0,0, 1],
                     data=0b0000111101110101)

    def test_8m1(self):
        self.dut = AsyncSerialRX(divisor=5, data_bits=8, parity="mark")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1, 1], data=0b01110101)
        self.rx_test([0, 1,0,1,0,1,1,0,0, 1, 1], data=0b00110101)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 0, 1], errors={"parity"})

    def test_8s1(self):
        self.dut = AsyncSerialRX(divisor=5, data_bits=8, parity="space")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 0, 1], data=0b01110101)
        self.rx_test([0, 1,0,1,0,1,1,0,0, 0, 1], data=0b00110101)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1, 1], errors={"parity"})

    def test_8e1(self):
        self.dut = AsyncSerialRX(divisor=5, data_bits=8, parity="even")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1, 1], data=0b01110101)
        self.rx_test([0, 1,0,1,0,1,1,0,0, 0, 1], data=0b00110101)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 0, 1], errors={"parity"})

    def test_8o1(self):
        self.dut = AsyncSerialRX(divisor=5, data_bits=8, parity="odd")
        self.rx_test([0, 1,0,1,0,1,1,1,0, 0, 1], data=0b01110101)
        self.rx_test([0, 1,0,1,0,1,1,0,0, 1, 1], data=0b00110101)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1, 1], errors={"parity"})

    def test_8n1_pins(self):
        pins = _DummyPins()
        self.dut = AsyncSerialRX(divisor=5, data_bits=8, parity="none", pins=pins)
        self.rx_test([0, 1,0,1,0,1,1,1,0, 1], data=0b01110101, pins=pins)

    def test_err_frame(self):
        self.dut = AsyncSerialRX(divisor=5)
        self.rx_test([0, 0,0,0,0,0,0,0,0, 0], errors={"frame"})

    def test_err_overflow(self):
        self.dut = AsyncSerialRX(divisor=5)
        def process():
            self.assertFalse((yield self.dut.rdy))
            yield from self.tx_bits([0, 0,0,0,0,0,0,0,0, 1])
            yield from self.tx_period()
            yield
            self.assertFalse((yield self.dut.rdy))
            self.assertTrue((yield self.dut.err.overflow))
        simulation_test(self.dut, process)

    def test_fifo(self):
        self.dut  = AsyncSerialRX(divisor=5)
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
            self.assertTrue((yield self.fifo.r_rdy))
            self.assertEqual((yield self.fifo.r_data), 0x55)
            yield self.fifo.r_en.eq(1)
            yield
            yield
            while not (yield self.fifo.r_rdy):
                yield
            self.assertEqual((yield self.fifo.r_data), 0xAA)
            yield
            self.assertFalse((yield self.fifo.r_rdy))
        simulation_test(m, process)


class AsyncSerialTXSignatureTestCase(TestCase):
    def test_simple(self):
        sig = AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 8)
        self.assertEqual(sig.data_bits, 7)
        self.assertEqual(sig.parity, Parity.EVEN)
        self.assertEqual(sig.members, Signature({
            "divisor": In(unsigned(8), reset=10),
            "data":    In(unsigned(7)),
            "rdy":     Out(unsigned(1)),
            "ack":     In(unsigned(1)),
            "o":       Out(unsigned(1), reset=1),
        }).members)

    def test_defaults(self):
        sig = AsyncSerialTX.Signature(divisor=10)
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 4)
        self.assertEqual(sig.data_bits, 8)
        self.assertEqual(sig.parity, Parity.NONE)

    def test_eq(self):
        self.assertEqual(AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                         AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"))
        # different divisor
        self.assertNotEqual(AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerialTX.Signature(divisor= 8, divisor_bits=8, data_bits=7, parity="even"))
        # different divisor_bits
        self.assertNotEqual(AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerialTX.Signature(divisor=10, divisor_bits=4, data_bits=7, parity="even"))
        # different data_bits
        self.assertNotEqual(AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="even"))
        # different parity
        self.assertNotEqual(AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="even"),
                            AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="none"))

    def test_repr(self):
        sig = AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(repr(sig), "AsyncSerialTX.Signature(SignatureMembers({"
                                        "'divisor': In(unsigned(8), reset=10), "
                                        "'data': In(unsigned(7)), "
                                        "'rdy': Out(unsigned(1)), "
                                        "'ack': In(unsigned(1)), "
                                        "'o': Out(unsigned(1), reset=1)}))")

    def test_wrong_divisor(self):
        with self.assertRaisesRegex(TypeError,
                r"Divisor initial value must be an integer greater than or equal to 1, not 0"):
            AsyncSerialTX.Signature(divisor=0)
        with self.assertRaisesRegex(TypeError,
                r"Divisor initial value must be an integer greater than or equal to 1, not 0"):
            AsyncSerialTX.Signature.check_parameters(divisor=0)

    def test_wrong_divisor_bits(self):
        with self.assertRaisesRegex(TypeError,
                r"Divisor bits must be an integer greater than or equal to 4, not 3"):
            AsyncSerialTX.Signature(divisor=8, divisor_bits=3)
        with self.assertRaisesRegex(TypeError,
                r"Divisor bits must be an integer greater than or equal to 4, not 3"):
            AsyncSerialTX.Signature.check_parameters(divisor=8, divisor_bits=3)

    def test_wrong_data_bits(self):
        with self.assertRaisesRegex(TypeError,
                r"Data bits must be a non-negative integer, not -1"):
            AsyncSerialTX.Signature(divisor=10, data_bits=-1)
        with self.assertRaisesRegex(TypeError,
                r"Data bits must be a non-negative integer, not -1"):
            AsyncSerialTX.Signature.check_parameters(divisor=10, data_bits=-1)

    def test_wrong_parity(self):
        with self.assertRaisesRegex(ValueError, r"'foo' is not a valid Parity"):
            AsyncSerialTX.Signature(divisor=10, parity="foo")
        with self.assertRaisesRegex(ValueError, r"'foo' is not a valid Parity"):
            AsyncSerialTX.Signature.check_parameters(divisor=10, parity="foo")


class AsyncSerialTXTestCase(TestCase):
    def tx_period(self):
        for _ in range((yield self.dut.divisor)):
            yield

    def tx_test(self, data, *, bits, pins=None):
        if pins is not None:
            tx_o = pins.tx.o
        else:
            tx_o = self.dut.o
        def process():
            self.assertTrue((yield self.dut.rdy))
            yield self.dut.data.eq(data)
            yield self.dut.ack.eq(1)
            while (yield self.dut.rdy):
                yield
            for bit in bits:
                yield from self.tx_period()
                self.assertEqual((yield tx_o), bit)
        simulation_test(self.dut, process)

    def test_signature(self):
        dut = AsyncSerialTX(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertIsInstance(dut.signature, AsyncSerialTX.Signature)
        self.assertEqual(dut.signature.divisor, 10)
        self.assertEqual(dut.signature.divisor_bits, 8)
        self.assertEqual(dut.signature.data_bits, 7)
        self.assertEqual(dut.signature.parity, Parity.EVEN)

    def test_defaults(self):
        dut = AsyncSerialTX(divisor=10)
        self.assertEqual(dut.signature.divisor, 10)
        self.assertEqual(dut.signature.divisor_bits, 4)
        self.assertEqual(dut.signature.data_bits, 8)
        self.assertEqual(dut.signature.parity, Parity.NONE)

    def test_8n1(self):
        self.dut = AsyncSerialTX(divisor=1, data_bits=8, parity="none")
        self.tx_test(0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 1])

    def test_16n1(self):
        self.dut = AsyncSerialTX(divisor=1, data_bits=16, parity="none")
        self.tx_test(0b0000111101110101, bits=[0, 1,0,1,0,1,1,1,0,1,1,1,1,0,0,0,0, 1])

    def test_8m1(self):
        self.dut = AsyncSerialTX(divisor=1, data_bits=8, parity="mark")
        self.tx_test(0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 1, 1])
        self.tx_test(0b00110101, bits=[0, 1,0,1,0,1,1,0,0, 1, 1])

    def test_8s1(self):
        self.dut = AsyncSerialTX(divisor=1, data_bits=8, parity="space")
        self.tx_test(0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 0, 1])
        self.tx_test(0b00110101, bits=[0, 1,0,1,0,1,1,0,0, 0, 1])

    def test_8e1(self):
        self.dut = AsyncSerialTX(divisor=1, data_bits=8, parity="even")
        self.tx_test(0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 1, 1])
        self.tx_test(0b00110101, bits=[0, 1,0,1,0,1,1,0,0, 0, 1])

    def test_8o1(self):
        self.dut = AsyncSerialTX(divisor=1, data_bits=8, parity="odd")
        self.tx_test(0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 0, 1])
        self.tx_test(0b00110101, bits=[0, 1,0,1,0,1,1,0,0, 1, 1])

    def test_8n1_pins(self):
        pins = _DummyPins()
        self.dut = AsyncSerialTX(divisor=1, data_bits=8, parity="none", pins=pins)
        self.tx_test(0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 1], pins=pins)

    def test_fifo(self):
        self.dut  = AsyncSerialTX(divisor=1)
        self.fifo = SyncFIFO(width=8, depth=4)
        m = Module()
        m.submodules.tx   = self.dut
        m.submodules.fifo = self.fifo
        m.d.comb += [
            self.dut.ack.eq(self.fifo.r_rdy),
            self.dut.data.eq(self.fifo.r_data),
            self.fifo.r_en.eq(self.fifo.r_rdy & self.dut.rdy),
        ]
        def process():
            self.assertTrue((yield self.fifo.w_rdy))
            yield self.fifo.w_en.eq(1)
            yield self.fifo.w_data.eq(0x55)
            yield
            self.assertTrue((yield self.fifo.w_rdy))
            yield self.fifo.w_data.eq(0xAA)
            yield
            yield self.fifo.w_en.eq(0)
            yield
            for bit in [0, 1,0,1,0,1,0,1,0, 1]:
                yield from self.tx_period()
                self.assertEqual((yield self.dut.o), bit)
            yield
            for bit in [0, 0,1,0,1,0,1,0,1, 1]:
                yield from self.tx_period()
                self.assertEqual((yield self.dut.o), bit)
        simulation_test(m, process)


class AsyncSerialSignatureTestCase(TestCase):
    def test_simple(self):
        sig = AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 8)
        self.assertEqual(sig.data_bits, 7)
        self.assertEqual(sig.parity, Parity.EVEN)
        self.assertEqual(sig.members, Signature({
            "divisor": In(unsigned(8), reset=10),
            "rx": Out(AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")),
            "tx": Out(AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")),
        }).members)

    def test_defaults(self):
        sig = AsyncSerial.Signature(divisor=10)
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 4)
        self.assertEqual(sig.data_bits, 8)
        self.assertEqual(sig.parity, Parity.NONE)

    def test_eq(self):
        self.assertEqual(AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                         AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"))
        # different divisor
        self.assertNotEqual(AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerial.Signature(divisor= 8, divisor_bits=8, data_bits=7, parity="even"))
        # different divisor_bits
        self.assertNotEqual(AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerial.Signature(divisor=10, divisor_bits=4, data_bits=7, parity="even"))
        # different data_bits
        self.assertNotEqual(AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even"),
                            AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="even"))
        # different parity
        self.assertNotEqual(AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="even"),
                            AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=8, parity="none"))

    def test_repr(self):
        sig = AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(repr(sig), "AsyncSerial.Signature(SignatureMembers({"
                                        "'divisor': In(unsigned(8), reset=10), "
                                        "'rx': Out(AsyncSerialRX.Signature(SignatureMembers({"
                                            "'divisor': In(unsigned(8), reset=10), "
                                            "'data': Out(unsigned(7)), "
                                            "'err': Out(StructLayout({"
                                                "'overflow': 1, "
                                                "'frame': 1, "
                                                "'parity': 1})), "
                                            "'rdy': Out(unsigned(1)), "
                                            "'ack': In(unsigned(1)), "
                                            "'i': In(unsigned(1), reset=1)}))), "
                                        "'tx': Out(AsyncSerialTX.Signature(SignatureMembers({"
                                            "'divisor': In(unsigned(8), reset=10), "
                                            "'data': In(unsigned(7)), "
                                            "'rdy': Out(unsigned(1)), "
                                            "'ack': In(unsigned(1)), "
                                            "'o': Out(unsigned(1), reset=1)})))}))")

    def test_wrong_divisor(self):
        with self.assertRaisesRegex(TypeError,
                r"Divisor initial value must be an integer greater than or equal to 5, not 4"):
            AsyncSerial.Signature(divisor=4)
        with self.assertRaisesRegex(TypeError,
                r"Divisor initial value must be an integer greater than or equal to 5, not 4"):
            AsyncSerial.Signature.check_parameters(divisor=4)

    def test_wrong_divisor_bits(self):
        with self.assertRaisesRegex(TypeError,
                r"Divisor bits must be an integer greater than or equal to 4, not 3"):
            AsyncSerial.Signature(divisor=8, divisor_bits=3)
        with self.assertRaisesRegex(TypeError,
                r"Divisor bits must be an integer greater than or equal to 4, not 3"):
            AsyncSerial.Signature.check_parameters(divisor=8, divisor_bits=3)

    def test_wrong_data_bits(self):
        with self.assertRaisesRegex(TypeError,
                r"Data bits must be a non-negative integer, not -1"):
            AsyncSerial.Signature(divisor=10, data_bits=-1)
        with self.assertRaisesRegex(TypeError,
                r"Data bits must be a non-negative integer, not -1"):
            AsyncSerial.Signature.check_parameters(divisor=10, data_bits=-1)

    def test_wrong_parity(self):
        with self.assertRaisesRegex(ValueError, r"'foo' is not a valid Parity"):
            AsyncSerial.Signature(divisor=10, parity="foo")
        with self.assertRaisesRegex(ValueError, r"'foo' is not a valid Parity"):
            AsyncSerial.Signature.check_parameters(divisor=10, parity="foo")


class AsyncSerialTestCase(TestCase):
    def test_signature(self):
        dut = AsyncSerial(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertIsInstance(dut.signature, AsyncSerial.Signature)
        self.assertEqual(dut.signature.divisor, 10)
        self.assertEqual(dut.signature.divisor_bits, 8)
        self.assertEqual(dut.signature.data_bits, 7)
        self.assertEqual(dut.signature.parity, Parity.EVEN)

    def test_defaults(self):
        dut = AsyncSerial(divisor=10)
        self.assertEqual(dut.signature.divisor, 10)
        self.assertEqual(dut.signature.divisor_bits, 4)
        self.assertEqual(dut.signature.data_bits, 8)
        self.assertEqual(dut.signature.parity, Parity.NONE)

    def test_loopback(self):
        self.dut = AsyncSerial(divisor=5)
        m = Module()
        m.submodules.serial = self.dut
        m.d.comb += self.dut.rx.i.eq(self.dut.tx.o)
        def process():
            self.assertTrue((yield self.dut.tx.rdy))
            yield self.dut.tx.data.eq(0xAA)
            yield self.dut.tx.ack.eq(1)
            yield
            yield self.dut.tx.ack.eq(0)
            yield self.dut.rx.ack.eq(1)
            yield
            while not (yield self.dut.rx.rdy):
                yield
            self.assertEqual((yield self.dut.rx.data), 0xAA)
        simulation_test(m, process)

    def test_loopback_pins(self):
        pins = _DummyPins()
        self.dut = AsyncSerial(divisor=5, pins=pins)
        m = Module()
        m.submodules.serial = self.dut
        m.d.comb += pins.rx.i.eq(pins.tx.o)
        def process():
            self.assertTrue((yield self.dut.tx.rdy))
            yield self.dut.tx.data.eq(0xAA)
            yield self.dut.tx.ack.eq(1)
            yield
            yield self.dut.tx.ack.eq(0)
            yield self.dut.rx.ack.eq(1)
            yield
            while not (yield self.dut.rx.rdy):
                yield
            self.assertEqual((yield self.dut.rx.data), 0xAA)
        simulation_test(m, process)
