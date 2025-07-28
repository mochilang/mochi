import java.math.BigInteger

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

fun pow10(n: Int): Double {
    var r: Double = 1.0
    var i: Int = 0
    while (i < n) {
        r = r * 10.0
        i = i + 1
    }
    return r
}

fun powf(base: Double, exp: Double): Double {
    if (::exp == 0.5) {
        var guess: Double = base
        var i: Int = 0
        while (i < 20) {
            guess = (guess + (base / guess)) / 2.0
            i = i + 1
        }
        return guess
    }
    var result: Double = 1.0
    var n: Int = ::exp.toInt()
    var i: Int = 0
    while (i < n) {
        result = result * base
        i = i + 1
    }
    return result
}

fun formatFloat(f: Double, prec: Int): String {
    val scale: Double = pow10(prec)
    val scaled: Double = (f * scale) + 0.5
    var n: Int = scaled.toInt()
    var digits: String = n.toString()
    while (digits.length <= prec) {
        digits = "0" + digits
    }
    val intPart: String = digits.substring(0, digits.length - prec)
    val fracPart: String = digits.substring(digits.length - prec, digits.length)
    return (intPart + ".") + fracPart
}

fun padLeft(s: String, w: Int): String {
    var res: String = ""
    var n: BigInteger = (w - s.length).toBigInteger()
    while (n.compareTo(0.toBigInteger()) > 0) {
        res = res + " "
        n = n.subtract(1.toBigInteger())
    }
    return res + s
}

fun rowString(row: MutableList<Double>): String {
    var s: String = "["
    var i: Int = 0
    while (i < row.size) {
        s = s + padLeft(formatFloat(row[i], 3), 6)
        if (i < (row.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    return s + "] "
}

fun printMatrix(heading: String, m: MutableList<MutableList<Double>>): Unit {
    println(heading)
    var i: Int = 0
    while (i < m.size) {
        println(rowString(m[i]))
        i = i + 1
    }
}

fun elementWiseMM(m1: MutableList<MutableList<Double>>, m2: MutableList<MutableList<Double>>, f: (Double, Double) -> Double): MutableList<MutableList<Double>> {
    var z: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var r: Int = 0
    while (r < m1.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = 0
        while (c < (m1[r]).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((f(m1[r][c], m2[r][c])).toDouble()); _tmp } as MutableList<Double>
            c = c + 1
        }
        z = run { val _tmp = z.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Double>>
        r = r + 1
    }
    return z
}

fun elementWiseMS(m: MutableList<MutableList<Double>>, s: Double, f: (Double, Double) -> Double): MutableList<MutableList<Double>> {
    var z: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var r: Int = 0
    while (r < m.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = 0
        while (c < (m[r]).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((f(m[r][c], s)).toDouble()); _tmp } as MutableList<Double>
            c = c + 1
        }
        z = run { val _tmp = z.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Double>>
        r = r + 1
    }
    return z
}

fun add(a: Double, b: Double): Double {
    return a + b
}

fun sub(a: Double, b: Double): Double {
    return a - b
}

fun mul(a: Double, b: Double): Double {
    return a * b
}

fun div(a: Double, b: Double): Double {
    return a / b
}

fun exp(a: Double, b: Double): Double {
    return powf(a, b)
}

fun user_main(): Unit {
    val m1: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(3.0, 1.0, 4.0), mutableListOf(1.0, 5.0, 9.0))
    val m2: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.0, 7.0, 1.0), mutableListOf(8.0, 2.0, 8.0))
    printMatrix("m1:", m1)
    printMatrix("m2:", m2)
    println("")
    printMatrix("m1 + m2:", elementWiseMM(m1, m2, ::add))
    printMatrix("m1 - m2:", elementWiseMM(m1, m2, ::sub))
    printMatrix("m1 * m2:", elementWiseMM(m1, m2, ::mul))
    printMatrix("m1 / m2:", elementWiseMM(m1, m2, ::div))
    printMatrix("m1 ^ m2:", elementWiseMM(m1, m2, ::exp))
    println("")
    val s: Double = 0.5
    println("s: " + s.toString())
    printMatrix("m1 + s:", elementWiseMS(m1, s, ::add))
    printMatrix("m1 - s:", elementWiseMS(m1, s, ::sub))
    printMatrix("m1 * s:", elementWiseMS(m1, s, ::mul))
    printMatrix("m1 / s:", elementWiseMS(m1, s, ::div))
    printMatrix("m1 ^ s:", elementWiseMS(m1, s, ::exp))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
