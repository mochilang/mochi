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

fun averageSquareDiff(f: Double, preds: MutableList<Double>): Double {
    var av: Double = 0.0
    var i: Int = 0
    while (i < preds.size) {
        av = av + ((preds[i] - f) * (preds[i] - f))
        i = i + 1
    }
    av = av / preds.size.toDouble()
    return av
}

fun diversityTheorem(truth: Double, preds: MutableList<Double>): MutableList<Double> {
    var av: Double = 0.0
    var i: Int = 0
    while (i < preds.size) {
        av = av + preds[i]
        i = i + 1
    }
    av = av / preds.size.toDouble()
    val avErr: Double = averageSquareDiff(truth, preds)
    val crowdErr: Double = (truth - av) * (truth - av)
    val div: Double = averageSquareDiff(av, preds)
    return mutableListOf(avErr, crowdErr, div)
}

fun user_main(): Unit {
    val predsArray: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(48.0, 47.0, 51.0), mutableListOf(48.0, 47.0, 51.0, 42.0))
    val truth: Double = 49.0
    var i: Int = 0
    while (i < predsArray.size) {
        val preds: MutableList<Double> = predsArray[i]
        val res: MutableList<Double> = diversityTheorem(truth, preds)
        println("Average-error : " + padLeft(formatFloat(res[0], 3), 6))
        println("Crowd-error   : " + padLeft(formatFloat(res[1], 3), 6))
        println("Diversity     : " + padLeft(formatFloat(res[2], 3), 6))
        println("")
        i = i + 1
    }
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
