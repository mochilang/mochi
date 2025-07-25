// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}
// Code generated from chinese-remainder-theorem.mochi

val n = mutableListOf(3, 5, 7)

val a = mutableListOf(2, 3, 2)

val res = crt(a, n)

/**
 * Auto-generated from Mochi
 * @param a Int
 * @param b Int
 * @return MutableList<Int>
 */
fun egcd(a: Int, b: Int): MutableList<Int> {
    if (a == 0) {
        return mutableListOf(b, 0, 1)
    }
    val res = egcd(b % a, a)
    val g = res[0]
    val x1 = res[1]
    val y1 = res[2]
    return mutableListOf(g, toDouble(y1) - toDouble(((b).toDouble() / (a).toDouble()) * toDouble(x1)), x1)
}

/**
 * Auto-generated from Mochi
 * @param a Int
 * @param m Int
 * @return Int
 */
fun modInv(a: Int, m: Int): Int {
    val r = egcd(a, m)
    if (r[0] != 1) {
        return 0
    }
    val x = r[1]
    if (x < 0) {
        return x + m
    }
    return x
}

/**
 * Auto-generated from Mochi
 * @param a MutableList<Int>
 * @param n MutableList<Int>
 * @return Int
 */
fun crt(a: MutableList<Int>, n: MutableList<Int>): Int {
    var prod = 1
    var i = 0
    while (i < n.size) {
        prod = prod * n[i]
        i = i + 1
    }
    var x = 0
    i = 0
    while (i < n.size) {
        val ni = n[i]
        val ai = a[i]
        val p = (prod).toDouble() / (ni).toDouble()
        val inv = modInv(p % (ni).toDouble(), ni)
        x = (x).toDouble() + (ai * inv).toDouble() * p
        i = i + 1
    }
    return x % prod
}

fun main() {
    println(res.toString() + " <nil>")
}
