import java.math.BigInteger

val THRESHOLD: Long = 140737488355328
fun indexOf(xs: MutableList<Int>, value: Int): Int {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i] == value) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun contains(xs: MutableList<Int>, value: Int): Boolean {
    return indexOf(xs, value) != (0 - 1)
}

fun maxOf(a: Int, b: Int): Int {
    if (a > b) {
        return a
    } else {
        return b
    }
}

fun intSqrt(n: Int): Int {
    if (n == 0) {
        return 0
    }
    var x: Int = n
    var y: BigInteger = (x + 1) / 2
    while ((y as Number).toDouble() < x) {
        x = y as Int
        y = (x + (n / x)) / 2
    }
    return x
}

fun sumProperDivisors(n: Int): Int {
    if (n < 2) {
        return 0
    }
    val sqrt: Int = intSqrt(n)
    var sum: Int = 1
    var i: Int = 2
    while (i <= sqrt) {
        if ((n % i) == 0) {
            sum = (sum + i) + (n / i)
        }
        i = i + 1
    }
    if ((sqrt * sqrt) == n) {
        sum = sum - sqrt
    }
    return sum
}

fun classifySequence(k: Int): MutableMap<String, Any?> {
    var last: Int = k
    var seq: MutableList<Int> = mutableListOf(k)
    while (true) {
        last = sumProperDivisors(last)
        seq = run { val _tmp = seq.toMutableList(); _tmp.add(last); _tmp } as MutableList<Int>
        val n: Int = seq.size
        var aliquot: String = ""
        if (last == 0) {
            aliquot = "Terminating"
        } else {
            if ((n == 2) && (last == k)) {
                aliquot = "Perfect"
            } else {
                if ((n == 3) && (last == k)) {
                    aliquot = "Amicable"
                } else {
                    if ((n >= 4) && (last == k)) {
                        aliquot = ("Sociable[" + (n - 1).toString()) + "]"
                    } else {
                        if (last == seq[n - 2]) {
                            aliquot = "Aspiring"
                        } else {
                            if ((contains(seq.subList(1, maxOf(1, n - 2)), last)) as Boolean) {
                                val idx: Int = indexOf(seq, last)
                                aliquot = ("Cyclic[" + ((n - 1) - idx).toString()) + "]"
                            } else {
                                if ((n == 16) || (last > THRESHOLD)) {
                                    aliquot = "Non-Terminating"
                                }
                            }
                        }
                    }
                }
            }
        }
        if (aliquot != "") {
            return mutableMapOf<String, Any?>("seq" to (seq), "aliquot" to (aliquot))
        }
    }
    return mutableMapOf<String, Any?>("seq" to (seq), "aliquot" to (""))
}

fun padLeft(n: Int, w: Int): String {
    var s: String = n.toString()
    while (s.length < w) {
        s = " " + s
    }
    return s
}

fun padRight(s: String, w: Int): String {
    var r: String = s
    while (r.length < w) {
        r = r + " "
    }
    return r
}

fun joinWithCommas(seq: MutableList<Int>): String {
    var s: String = "["
    var i: Int = 0
    while (i < seq.size) {
        s = s + (seq[i]).toString()
        if (i < (seq.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun user_main(): Unit {
    println("Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n")
    var k: Int = 1
    while (k <= 10) {
        val res: MutableMap<String, Any?> = classifySequence(k)
        println((((padLeft(k, 2) + ": ") + padRight(((res)["aliquot"] as Any?).toString(), 15)) + " ") + joinWithCommas(((res)["seq"] as Any?) as MutableList<Int>))
        k = k + 1
    }
    println("")
    val s: MutableList<Int> = mutableListOf(11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488)
    var i: Int = 0
    while (i < s.size) {
        val _val: Int = s[i]
        val res: MutableMap<String, Any?> = classifySequence(_val)
        println((((padLeft(_val, 7) + ": ") + padRight(((res)["aliquot"] as Any?).toString(), 15)) + " ") + joinWithCommas(((res)["seq"] as Any?) as MutableList<Int>))
        i = i + 1
    }
    println("")
    val big: Int = 15355717786080
    val r: MutableMap<String, Any?> = classifySequence(big)
    println((((big.toString() + ": ") + padRight(((r)["aliquot"] as Any?).toString(), 15)) + " ") + joinWithCommas(((r)["seq"] as Any?) as MutableList<Int>))
}

fun main() {
    user_main()
}
