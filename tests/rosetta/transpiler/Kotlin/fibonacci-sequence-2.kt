import java.math.BigInteger

fun fib(n: Int): BigInteger {
    if (n < 2) {
        return n.toBigInteger()
    }
    var a: BigInteger = java.math.BigInteger.valueOf(0)
    var b: BigInteger = java.math.BigInteger.valueOf(1)
    var i: Int = n
    i = i - 1
    while (i > 0) {
        var tmp: BigInteger = a.add(b)
        a = b
        b = tmp
        i = i - 1
    }
    return b
}

fun main() {
}
