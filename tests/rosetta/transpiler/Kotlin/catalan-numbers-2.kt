import java.math.BigInteger

fun catalanRec(n: Int): Int {
    if (n == 0) {
        return 1
    }
    var t1: BigInteger = (2 * n).toBigInteger()
    var t2: BigInteger = t1.subtract((1).toBigInteger())
    var t3: BigInteger = (2).toBigInteger().multiply((t2))
    var t5: BigInteger = t3.multiply((catalanRec(n - 1)).toBigInteger())
    return ((t5.divide((n + 1).toBigInteger())).toInt())
}

fun user_main(): Unit {
    for (i in 1 until 16) {
        println(catalanRec(i).toString())
    }
}

fun main() {
    user_main()
}
