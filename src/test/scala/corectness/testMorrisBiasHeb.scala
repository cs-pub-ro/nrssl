package corectness

import org.scalatest.funsuite.AnyFunSuite

class MorrisBiasHEB_Tests extends AnyFunSuite {

  test ("IEEE to MorrisBiasHEB - Mantissa Narrowing - No subnormals") {
    val seed = 123456789L
    val numSamples = 1e3.toInt

    // For standard MorrisBiasHEB on 32 bits, 5 bits are used up by the sign bit and exponent size field
    // So, we can only have 27 bits of mantissa + exponent, rather than 23 bits of mantissa and 8 bits of exponent for IEEE
    // Therefore, a MorrisBiasHEB number with less than 23 bits of mantissa will not be able to represent some IEEE numbers losslessly.

    val ieee_bias = 127
    val ieee_es = 8
    val ieee_fs = 23

    // Both representations use a sign bit, so we can skip testing both values
    val sign_bit = 0

    def build_float_as_int(fs: Int, exp: Int, mantissa: Int, overflow_bits: Int) = {
      val ieee_mantissa = (mantissa << (ieee_fs - fs)) + overflow_bits
      val ieee_int_val = sign_bit << (ieee_es + ieee_fs) | (exp << ieee_fs) | ieee_mantissa
      ieee_int_val
    }

    for {
      es <- 5 to 6
      min_abs_val = 1 << es
      max_abs_val = Math.min((1 << (es + 1)) - 1, 128)
      fs = 27 - es

      sign <- Seq(-1, 1)

      // Take max(-126,..) to avoid subnormals
      min_val = Math.max(-126, Math.min(min_abs_val * sign, max_abs_val * sign)) + ieee_bias
      max_val = Math.max(min_abs_val * sign, max_abs_val * sign) + ieee_bias

      overflow_bits <- 1 until (1 << (ieee_fs - fs))
    }
    {
      val exp_range = max_val - min_val + 1
      val mantissa_range = 1 << fs
      val total_space = exp_range.toLong * mantissa_range.toLong
      val effective_samples = Math.min(numSamples.toLong, total_space).toInt

      val rng = new scala.util.Random(seed)
      val seen = scala.collection.mutable.HashSet[Long]()

      for (_ <- 0 until effective_samples) {
        val exp = min_val + rng.nextInt(exp_range)
        val mantissa = rng.nextInt(mantissa_range)

        val key = exp.toLong * mantissa_range + mantissa
        if (!seen.contains(key)) {
          seen.add(key)

          val ieee_int_val = build_float_as_int(fs, exp, mantissa, overflow_bits)
          val ieee_float_val = java.lang.Float.intBitsToFloat(ieee_int_val)
          assert(
            Common.check_morris_bias_heb(ieee_float_val),
            s"Failed for fs: $fs, exp: $exp, mantissa: $mantissa, overflow_bits: $overflow_bits, " +
              s"int value: ${ieee_int_val}, float value: $ieee_float_val, " +
              s"min_val: $min_val, max_val: $max_val"
          )
        }
      }
    }
  }
}
