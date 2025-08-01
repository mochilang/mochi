// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}
// Code generated from averages-simple-moving-average.mochi

/**
 * Auto-generated from Mochi
 * @param s String
 * @param ch String
 * @return Int
 */
fun indexOf(s: String, ch: String): Int {
    var i = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return -1
}

/**
 * Auto-generated from Mochi
 * @param x Double
 * @return String
 */
fun fmt3(x: Double): String {
    var y = (((x * 1000.0) + 0.5)).toInt() as Double / 1000.0
    var s = y.toString()
    var dot = indexOf(s, ".")
    if (dot == 0 - 1) {
        s = s + ".000"
    }
    else {
        var decs = s.length - dot - 1
        if (decs > 3) {
            s = s.substring(0, dot + 4)
        }
        else {
            while (decs < 3) {
                s = s + "0"
                decs = decs + 1
            }
        }
    }
    return s
}

/**
 * Auto-generated from Mochi
 * @param s String
 * @param width Int
 * @return String
 */
fun pad(s: String, width: Int): String {
    var out = s
    while (out.length < width) {
        out = " " + out
    }
    return out
}

/**
 * Auto-generated from Mochi
 * @param xs MutableList<Double>
 * @param period Int
 * @return MutableList<Double>
 */
fun smaSeries(xs: MutableList<Double>, period: Int): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var sum = 0.0
    var i = 0
    while (i < xs.size) {
        sum = sum + xs[i]
        if (i >= period) {
            sum = sum - xs[i - period]
        }
        var denom = i + 1
        if (denom > period) {
            denom = period
        }
        res = append(res, sum / (denom as Double))
        i = i + 1
    }
    return res
}

/**
 * Auto-generated from Mochi
 */
fun main(): Unit {
    var xs = mutableListOf(1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0)
    var sma3 = smaSeries(xs, 3)
    var sma5 = smaSeries(xs, 5)
    println("x       sma3   sma5")
    var i = 0
    while (i < xs.size) {
        val line = pad(fmt3(xs[i]), 5) + "  " + pad(fmt3(sma3[i]), 5) + "  " + pad(fmt3(sma5[i]), 5)
        println(line)
        i = i + 1
    }
}

