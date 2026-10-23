package corectness

import org.scalatest.funsuite.AnyFunSuite
import ro.upb.nrs.sl._

// Encoding a value and decoding its bits must give the value back, and no
// non-zero value may encode as the zero or NR pattern.
class MorrisRoundTrip_Tests extends AnyFunSuite {
  val size = 32
  val g = 4
  val zero = "0" * size
  val nr = "1" + "0" * (size - 1)

  val values = Seq(1.0, -1.0, 2.0, -2.0, 0.5, -0.5, 1.5, -1.5, 3.0, 0.1, -0.1, 1000.0, 1234567.0, 1e-5, 3.14159)

  def check(name: String, enc: Double => FixedPrecisionNumberRepresentationSystem, dec: String => FixedPrecisionNumberRepresentationSystem): Unit = {
    for (v <- values) {
      val m = enc(v)
      val bits = m.toBinaryString
      assert(bits != zero && bits != nr, s"$name($v) encodes as $bits")
      assert(dec(bits).toDouble == m.toDouble, s"$name($v): $bits decodes to ${dec(bits).toDouble}, expected ${m.toDouble}")
    }
  }

  test("Morris round trip") {
    check("Morris", v => Morris(v, g, size, Morris.default_rounding), s => Morris(s, g, size, Morris.default_rounding))
  }

  test("MorrisHEB round trip") {
    check("MorrisHEB", v => MorrisHEB(v, g, size, MorrisHEB.default_rounding), s => MorrisHEB(s, g, size, MorrisHEB.default_rounding))
  }

  test("MorrisBiasHEB round trip") {
    check("MorrisBiasHEB", v => MorrisBiasHEB(v, g, size, MorrisBiasHEB.default_rounding), s => MorrisBiasHEB(s, g, size, MorrisBiasHEB.default_rounding))
  }

  test("MorrisUnaryHEB round trip") {
    check("MorrisUnaryHEB", v => MorrisUnaryHEB(v, size, MorrisUnaryHEB.default_rounding), s => MorrisUnaryHEB(s, size, MorrisUnaryHEB.default_rounding))
  }
}
