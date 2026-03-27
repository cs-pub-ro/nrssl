package corectness

class Utils {}

object Utils {
  def nextBinaryString(binStr: String): String = {
    val n = binStr.length
    val charArray = binStr.toCharArray

    var carry = 1
    for (i <- (n - 1) to 0 by -1 if carry == 1) {
      if (charArray(i) == '0') {
        charArray(i) = '1'
        carry = 0
      } else {
        charArray(i) = '0'
      }
    }

    if (carry == 1) {
      "1" + new String(charArray)
    } else {
      new String(charArray)
    }
  }

  def prevBinaryString(binStr: String): String = {
    val n = binStr.length
    val charArray = binStr.toCharArray

    var borrow = 1
    for (i <- (n - 1) to 0 by -1 if borrow == 1) {
      if (charArray(i) == '1') {
        charArray(i) = '0'
        borrow = 0
      } else {
        charArray(i) = '1'
      }
    }

    if (borrow == 1) {
      // Underflow case, return string of zeros
      "0" * n
    } else {
      new String(charArray)
    }
  }

  def naturalToBinary(n: Int, bitlen: Int): String = {
    val binaryStr = n.toBinaryString
    if (binaryStr.length > bitlen) {
      throw new IllegalArgumentException(s"Number $n cannot be represented in $bitlen bits.")
    }
    "0" * (bitlen - binaryStr.length) + binaryStr
  }
}