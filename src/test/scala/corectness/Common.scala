package corectness

import ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromRationalNumber
import ro.upb.nrs.sl.{FixedPrecisionNumberRepresentationSystem, IEEE754, IEEE754_B, MorrisBiasHEB, MorrisBiasHEB_B, MorrisUnaryHEB, RationalNumber_B}

class Common {
}

object Common {


  /*   Checks if the 'test' value is a better approximation of the 'ref' value than its neighbors
      'test_prev' and 'test_next'. This should be used only with systems that are binary comparable (i.e. the next floating point number available is always the binary neighbor).

      @param ref The reference RationalNumber_B value.
      @param test_prev The previous RationalNumber_B value to compare against.
      @param test The current RationalNumber_B value to check.
      @param test_next The next RationalNumber_B value to compare against.
      @return True if 'test' is a better approximation of 'ref' than both 'test_prev' and 'test_next', false otherwise.
  */
  def is_correct_approx(ref: RationalNumber_B, test_prev: RationalNumber_B, test: RationalNumber_B, test_next: RationalNumber_B): Boolean = {
    val diff_prev = (ref - test_prev).abs
    val diff = (ref - test).abs
    val diff_next = (ref - test_next).abs

    diff <= diff_prev && diff <= diff_next
  }


  def check_morris_bias_heb(ieee_float_val: Float): Boolean = {
    val ieee = IEEE754(ieee_float_val)
    val morris = fromRationalNumber(ieee.toRationalNumber, MorrisBiasHEB(0)).asInstanceOf[MorrisBiasHEB]

    def binstr_to_morris(s: String) = MorrisBiasHEB(s, morris.g_size, morris.size, morris.rounding)

    check_morris(ieee, morris, binstr_to_morris)
  }

  def check_morris_unary_heb(ieee_float_val: Float): Boolean = {
    val ieee = IEEE754(ieee_float_val)
    val morris = fromRationalNumber(ieee.toRationalNumber, MorrisUnaryHEB(0)).asInstanceOf[MorrisUnaryHEB]

    def binstr_to_morris(s: String) = MorrisUnaryHEB(s, morris.size, morris.rounding)

    check_morris(ieee, morris, binstr_to_morris)
  }

  def check_morris(ieee_val: IEEE754_B, morris_val: FixedPrecisionNumberRepresentationSystem, bin_str_to_morris: String => FixedPrecisionNumberRepresentationSystem): Boolean = {
    try {
      val morris = fromRationalNumber(ieee_val.toRationalNumber, morris_val).asInstanceOf[FixedPrecisionNumberRepresentationSystem]

      val morris_binary_repr = morris.toBinaryString

      val morris_prev = bin_str_to_morris(Utils.prevBinaryString(morris_binary_repr))
      val morris_next = bin_str_to_morris(Utils.nextBinaryString(morris_binary_repr))

      Common.is_correct_approx(ieee_val.toRationalNumber, morris_prev.toRationalNumber, morris.toRationalNumber, morris_next.toRationalNumber)
    }
    catch {
      case e: Throwable =>
        printf("Exception message: %s\n", e.getMessage)
        false
    }
  }


}