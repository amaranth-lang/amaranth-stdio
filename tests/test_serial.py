# amaranth: UnusedElaboratable=no

from unittest import TestCase

from amaranth import *
from amaranth.lib.data import StructLayout
from amaranth.lib.fifo import SyncFIFO
from amaranth.lib.wiring import *
from amaranth.sim import *

from amaranth_stdio.serial import *


class _DummyPins:
    def __init__(self):
        self.rx = Signal(StructLayout({"i": 1}), init={"i": 1})
        self.tx = Signal(StructLayout({"o": 1}), init={"o": 1})


class AsyncSerialRXSignatureTestCase(TestCase):
    def test_simple(self):
        sig = AsyncSerialRX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 8)
        self.assertEqual(sig.data_bits, 7)
        self.assertEqual(sig.parity, Parity.EVEN)
        self.assertEqual(sig.members, Signature({
            "divisor": In(unsigned(8), init=10),
            "data":    Out(unsigned(7)),
            "err":     Out(StructLayout({"overflow": 1, "frame": 1, "parity": 1})),
            "rdy":     Out(unsigned(1)),
            "ack":     In(unsigned(1)),
            "i":       In(unsigned(1), init=1),
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
                                        "'divisor': In(unsigned(8), init=10), "
                                        "'data': Out(unsigned(7)), "
                                        "'err': Out(StructLayout({"
                                            "'overflow': 1, "
                                            "'frame': 1, "
                                            "'parity': 1})), "
                                        "'rdy': Out(unsigned(1)), "
                                        "'ack': In(unsigned(1)), "
                                        "'i': In(unsigned(1), init=1)}))")

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
    async def _rx_period(self, ctx, dut):
        await ctx.tick().repeat(ctx.get(dut.divisor))

    async def _rx_bits(self, ctx, dut, bits, pins=None):
        rx_i = dut.i if pins is None else pins.rx.i
        for bit in bits:
            await self._rx_period(ctx, dut)
            ctx.set(rx_i, bit)

    def _rx_test(self, dut, bits, *, data=None, errors=None, pins=None):
        async def testbench(ctx):
            self.assertFalse(ctx.get(dut.rdy))
            ctx.set(dut.ack, 1)
            await self._rx_bits(ctx, dut, bits, pins)
            await ctx.tick().until(dut.rdy)
            if data is not None:
                self.assertFalse(ctx.get(dut.err.overflow))
                self.assertFalse(ctx.get(dut.err.frame))
                self.assertFalse(ctx.get(dut.err.parity))
                self.assertEqual(ctx.get(dut.data), data)
            if errors is not None:
                for error in errors:
                    self.assertTrue(ctx.get(getattr(dut.err, error)))

        sim = Simulator(dut)
        sim.add_clock(1e-6)
        sim.add_testbench(testbench)
        with sim.write_vcd(vcd_file="test.vcd"):
            sim.run()

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
        dut = AsyncSerialRX(divisor=5, data_bits=8, parity="none")
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 1], data=0b01110101)

    def test_16n1(self):
        dut = AsyncSerialRX(divisor=5, data_bits=16, parity="none")
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0,1,1,1,1,0,0,0,0, 1],
                      data=0b0000111101110101)

    def test_8m1(self):
        dut = AsyncSerialRX(divisor=5, data_bits=8, parity="mark")
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 1, 1], data=0b01110101)
        self._rx_test(dut, [0, 1,0,1,0,1,1,0,0, 1, 1], data=0b00110101)
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 0, 1], errors={"parity"})

    def test_8s1(self):
        dut = AsyncSerialRX(divisor=5, data_bits=8, parity="space")
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 0, 1], data=0b01110101)
        self._rx_test(dut, [0, 1,0,1,0,1,1,0,0, 0, 1], data=0b00110101)
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 1, 1], errors={"parity"})

    def test_8e1(self):
        dut = AsyncSerialRX(divisor=5, data_bits=8, parity="even")
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 1, 1], data=0b01110101)
        self._rx_test(dut, [0, 1,0,1,0,1,1,0,0, 0, 1], data=0b00110101)
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 0, 1], errors={"parity"})

    def test_8o1(self):
        dut = AsyncSerialRX(divisor=5, data_bits=8, parity="odd")
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 0, 1], data=0b01110101)
        self._rx_test(dut, [0, 1,0,1,0,1,1,0,0, 1, 1], data=0b00110101)
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 1, 1], errors={"parity"})

    def test_8n1_pins(self):
        pins = _DummyPins()
        dut = AsyncSerialRX(divisor=5, data_bits=8, parity="none", pins=pins)
        self._rx_test(dut, [0, 1,0,1,0,1,1,1,0, 1], data=0b01110101, pins=pins)

    def test_err_frame(self):
        dut = AsyncSerialRX(divisor=5)
        self._rx_test(dut, [0, 0,0,0,0,0,0,0,0, 0], errors={"frame"})

    def test_err_overflow(self):
        dut = AsyncSerialRX(divisor=5)

        async def testbench(ctx):
            self.assertFalse(ctx.get(dut.rdy))
            await self._rx_bits(ctx, dut, [0, 0,0,0,0,0,0,0,0, 1])
            await self._rx_period(ctx, dut)
            await ctx.tick()
            self.assertFalse(ctx.get(dut.rdy))
            self.assertTrue(ctx.get(dut.err.overflow))

        sim = Simulator(dut)
        sim.add_clock(1e-6)
        sim.add_testbench(testbench)
        with sim.write_vcd(vcd_file="test.vcd"):
            sim.run()

    def test_fifo(self):
        m = Module()
        m.submodules.dut  = dut  = AsyncSerialRX(divisor=5)
        m.submodules.fifo = fifo = SyncFIFO(width=8, depth=4)

        m.d.comb += [
            dut.ack.eq(fifo.w_rdy),
            fifo.w_en.eq(dut.rdy),
            fifo.w_data.eq(dut.data),
        ]

        async def testbench(ctx):
            await self._rx_bits(ctx, dut, [0, 1,0,1,0,1,0,1,0, 1,
                                           0, 0,1,0,1,0,1,0,1, 1])
            self.assertTrue(ctx.get(fifo.r_rdy))
            self.assertEqual(ctx.get(fifo.r_data), 0x55)
            ctx.set(fifo.r_en, 1)
            await ctx.tick()
            fifo_r_data_value, = await ctx.tick().sample(fifo.r_data).until(fifo.r_rdy)
            self.assertEqual(fifo_r_data_value, 0xAA)
            self.assertFalse(ctx.get(fifo.r_rdy))

        sim = Simulator(m)
        sim.add_clock(1e-6)
        sim.add_testbench(testbench)
        with sim.write_vcd(vcd_file="test.vcd"):
            sim.run()


class AsyncSerialTXSignatureTestCase(TestCase):
    def test_simple(self):
        sig = AsyncSerialTX.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 8)
        self.assertEqual(sig.data_bits, 7)
        self.assertEqual(sig.parity, Parity.EVEN)
        self.assertEqual(sig.members, Signature({
            "divisor": In(unsigned(8), init=10),
            "data":    In(unsigned(7)),
            "rdy":     Out(unsigned(1)),
            "ack":     In(unsigned(1)),
            "o":       Out(unsigned(1), init=1),
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
                                        "'divisor': In(unsigned(8), init=10), "
                                        "'data': In(unsigned(7)), "
                                        "'rdy': Out(unsigned(1)), "
                                        "'ack': In(unsigned(1)), "
                                        "'o': Out(unsigned(1), init=1)}))")

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
    async def _tx_period(self, ctx, dut):
        await ctx.tick().repeat(ctx.get(dut.divisor))

    def _tx_test(self, dut, data, *, bits, pins=None):
        tx_o = dut.o if pins is None else pins.tx.o

        async def testbench(ctx):
            self.assertTrue(ctx.get(dut.rdy))
            ctx.set(dut.data, data)
            ctx.set(dut.ack, 1)
            await ctx.tick().until(dut.rdy)
            for bit in bits:
                await self._tx_period(ctx, dut)
                self.assertEqual(ctx.get(tx_o), bit)

        sim = Simulator(dut)
        sim.add_clock(1e-6)
        sim.add_testbench(testbench)
        with sim.write_vcd(vcd_file="test.vcd"):
            sim.run()

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
        dut = AsyncSerialTX(divisor=1, data_bits=8, parity="none")
        self._tx_test(dut, 0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 1])

    def test_16n1(self):
        dut = AsyncSerialTX(divisor=1, data_bits=16, parity="none")
        self._tx_test(dut, 0b0000111101110101, bits=[0, 1,0,1,0,1,1,1,0,1,1,1,1,0,0,0,0, 1])

    def test_8m1(self):
        dut = AsyncSerialTX(divisor=1, data_bits=8, parity="mark")
        self._tx_test(dut, 0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 1, 1])
        self._tx_test(dut, 0b00110101, bits=[0, 1,0,1,0,1,1,0,0, 1, 1])

    def test_8s1(self):
        dut = AsyncSerialTX(divisor=1, data_bits=8, parity="space")
        self._tx_test(dut, 0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 0, 1])
        self._tx_test(dut, 0b00110101, bits=[0, 1,0,1,0,1,1,0,0, 0, 1])

    def test_8e1(self):
        dut = AsyncSerialTX(divisor=1, data_bits=8, parity="even")
        self._tx_test(dut, 0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 1, 1])
        self._tx_test(dut, 0b00110101, bits=[0, 1,0,1,0,1,1,0,0, 0, 1])

    def test_8o1(self):
        dut = AsyncSerialTX(divisor=1, data_bits=8, parity="odd")
        self._tx_test(dut, 0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 0, 1])
        self._tx_test(dut, 0b00110101, bits=[0, 1,0,1,0,1,1,0,0, 1, 1])

    def test_8n1_pins(self):
        pins = _DummyPins()
        dut = AsyncSerialTX(divisor=1, data_bits=8, parity="none", pins=pins)
        self._tx_test(dut, 0b01110101, bits=[0, 1,0,1,0,1,1,1,0, 1], pins=pins)

    def test_fifo(self):
        m = Module()
        m.submodules.tx   = dut  = AsyncSerialTX(divisor=1)
        m.submodules.fifo = fifo = SyncFIFO(width=8, depth=4)

        m.d.comb += [
            dut.ack.eq(fifo.r_rdy),
            dut.data.eq(fifo.r_data),
            fifo.r_en.eq(fifo.r_rdy & dut.rdy),
        ]

        async def testbench(ctx):
            self.assertTrue(ctx.get(fifo.w_rdy))
            ctx.set(fifo.w_en, 1)
            ctx.set(fifo.w_data, 0x55)
            await ctx.tick()
            self.assertTrue(ctx.get(fifo.w_rdy))
            ctx.set(fifo.w_data, 0xAA)
            await ctx.tick()
            ctx.set(fifo.w_en, 0)
            for bit in [0, 1,0,1,0,1,0,1,0, 1]:
                await self._tx_period(ctx, dut)
                self.assertEqual(ctx.get(dut.o), bit)
            await ctx.tick()
            for bit in [0, 0,1,0,1,0,1,0,1, 1]:
                await self._tx_period(ctx, dut)
                self.assertEqual(ctx.get(dut.o), bit)

        sim = Simulator(m)
        sim.add_clock(1e-6)
        sim.add_testbench(testbench)
        with sim.write_vcd(vcd_file="test.vcd"):
            sim.run()


class AsyncSerialSignatureTestCase(TestCase):
    def test_simple(self):
        sig = AsyncSerial.Signature(divisor=10, divisor_bits=8, data_bits=7, parity="even")
        self.assertEqual(sig.divisor, 10)
        self.assertEqual(sig.divisor_bits, 8)
        self.assertEqual(sig.data_bits, 7)
        self.assertEqual(sig.parity, Parity.EVEN)
        self.assertEqual(sig.members, Signature({
            "divisor": In(unsigned(8), init=10),
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
                                        "'divisor': In(unsigned(8), init=10), "
                                        "'rx': Out(AsyncSerialRX.Signature(SignatureMembers({"
                                            "'divisor': In(unsigned(8), init=10), "
                                            "'data': Out(unsigned(7)), "
                                            "'err': Out(StructLayout({"
                                                "'overflow': 1, "
                                                "'frame': 1, "
                                                "'parity': 1})), "
                                            "'rdy': Out(unsigned(1)), "
                                            "'ack': In(unsigned(1)), "
                                            "'i': In(unsigned(1), init=1)}))), "
                                        "'tx': Out(AsyncSerialTX.Signature(SignatureMembers({"
                                            "'divisor': In(unsigned(8), init=10), "
                                            "'data': In(unsigned(7)), "
                                            "'rdy': Out(unsigned(1)), "
                                            "'ack': In(unsigned(1)), "
                                            "'o': Out(unsigned(1), init=1)})))}))")

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
        m = Module()
        m.submodules.dut = dut = AsyncSerial(divisor=5)

        m.d.comb += dut.rx.i.eq(dut.tx.o)

        async def testbench(ctx):
            self.assertTrue(ctx.get(dut.tx.rdy))
            ctx.set(dut.tx.data, 0xAA)
            ctx.set(dut.tx.ack, 1)
            await ctx.tick()
            ctx.set(dut.tx.ack, 0)
            ctx.set(dut.rx.ack, 1)
            await ctx.tick()
            await ctx.tick().until(dut.rx.rdy)
            self.assertEqual(ctx.get(dut.rx.data), 0xAA)

        sim = Simulator(m)
        sim.add_clock(1e-6)
        sim.add_testbench(testbench)
        with sim.write_vcd(vcd_file="test.vcd"):
            sim.run()

    def test_loopback_pins(self):
        pins = _DummyPins()

        m = Module()
        m.submodules.dut = dut = AsyncSerial(divisor=5, pins=pins)

        m.d.comb += pins.rx.i.eq(pins.tx.o)

        async def testbench(ctx):
            self.assertTrue(ctx.get(dut.tx.rdy))
            ctx.set(dut.tx.data, 0xAA)
            ctx.set(dut.tx.ack, 1)
            await ctx.tick()
            ctx.set(dut.tx.ack, 0)
            ctx.set(dut.rx.ack, 1)
            await ctx.tick()
            await ctx.tick().until(dut.rx.rdy)
            self.assertEqual(ctx.get(dut.rx.data), 0xAA)

        sim = Simulator(m)
        sim.add_clock(1e-6)
        sim.add_testbench(testbench)
        with sim.write_vcd(vcd_file="test.vcd"):
            sim.run()
