import java.math.BigInteger

var e1: Int = pow_int(3, 2)
var e2: Int = pow_int(4, e1)
var base: BigInteger = java.math.BigInteger.valueOf(5)
var x: BigInteger = pow_big(base, e2)
var s: String = x.toString()
fun pow_int(base: Int, exp: Int): Int {
    var result: Int = 1
    var b: Int = base
    var e: Int = exp
    while (e > 0) {
        if ((e % 2) == 1) {
            result = result * b
        }
        b = b * b
        e = e / 2.toInt()
    }
    return result
}

fun pow_big(base: BigInteger, exp: Int): BigInteger {
    var result: BigInteger = java.math.BigInteger.valueOf(1)
    var b: BigInteger = base
    var e: Int = exp
    while (e > 0) {
        if ((e % 2) == 1) {
            result = result.multiply(b)
        }
        b = b.multiply(b)
        e = e / 2.toInt()
    }
    return result
}

fun main() {
    println(listOf("5^(4^(3^2)) has", s.length, "digits:", s.substring(0, 20), "...", s.substring(s.length - 20, s.length)).joinToString(" "))
}
