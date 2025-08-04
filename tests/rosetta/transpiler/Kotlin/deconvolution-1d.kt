import java.math.BigInteger

fun listToStringInts(xs: MutableList<Double>): String {
    var s: String = "["
    var i: Int = 0
    while (i < xs.size) {
        s = s + (((xs[i]!!).toInt())).toString()
        if (i < (xs.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    return s + "]"
}

fun deconv(g: MutableList<Double>, f: MutableList<Double>): MutableList<Double> {
    var h: MutableList<Double> = mutableListOf<Double>()
    var n: Int = 0
    var hn: BigInteger = ((g.size - f.size) + 1).toBigInteger()
    while ((n).toBigInteger().compareTo((hn)) < 0) {
        var v: Double = g[n]!!
        var lower: Int = 0
        if (n >= f.size) {
            lower = (n - f.size) + 1
        }
        var i: Int = lower
        while (i < n) {
            v = v - (h[i]!! * f[n - i]!!)
            i = i + 1
        }
        v = v / f[0]!!
        h = run { val _tmp = h.toMutableList(); _tmp.add(v); _tmp }
        n = n + 1
    }
    return h
}

fun user_main(): Unit {
    var h: MutableList<Double> = mutableListOf(0.0 - 8.0, 0.0 - 9.0, 0.0 - 3.0, 0.0 - 1.0, 0.0 - 6.0, 7.0)
    var f: MutableList<Double> = mutableListOf(0.0 - 3.0, 0.0 - 6.0, 0.0 - 1.0, 8.0, 0.0 - 6.0, 3.0, 0.0 - 1.0, 0.0 - 9.0, 0.0 - 9.0, 3.0, 0.0 - 2.0, 5.0, 2.0, 0.0 - 2.0, 0.0 - 7.0, 0.0 - 1.0)
    var g: MutableList<Double> = mutableListOf(24.0, 75.0, 71.0, 0.0 - 34.0, 3.0, 22.0, 0.0 - 45.0, 23.0, 245.0, 25.0, 52.0, 25.0, 0.0 - 67.0, 0.0 - 96.0, 96.0, 31.0, 55.0, 36.0, 29.0, 0.0 - 43.0, 0.0 - 7.0)
    println(listToStringInts(h))
    println(listToStringInts(deconv(g, f)))
    println(listToStringInts(f))
    println(listToStringInts(deconv(g, h)))
}

fun main() {
    user_main()
}
