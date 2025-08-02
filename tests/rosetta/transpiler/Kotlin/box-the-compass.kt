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

var compassPoint: MutableList<String> = mutableListOf("North", "North by east", "North-northeast", "Northeast by north", "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west")
var headings: MutableList<Double> = mutableListOf(0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38)
fun padLeft(s: String, w: Int): String {
    var res: String = ""
    var n: BigInteger = (w - s.length).toBigInteger()
    while (n.compareTo((0).toBigInteger()) > 0) {
        res = res + " "
        n = n.subtract((1).toBigInteger())
    }
    return res + s
}

fun padRight(s: String, w: Int): String {
    var out: String = s
    var i: Int = s.length
    while (i < w) {
        out = out + " "
        i = i + 1
    }
    return out
}

fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun format2(f: Double): String {
    var s: String = f.toString()
    var idx: Int = s.indexOf(".")
    if (idx < 0) {
        s = s + ".00"
    } else {
        var need: BigInteger = (idx + 3).toBigInteger()
        if ((s.length).toBigInteger().compareTo((need)) > 0) {
            s = s.substring(0, (need).toInt())
        } else {
            while ((s.length).toBigInteger().compareTo((need)) < 0) {
                s = s + "0"
            }
        }
    }
    return s
}

fun cpx(h: Double): Int {
    var x: Int = (((h / 11.25) + 0.5).toInt())
    x = Math.floorMod(x, 32)
    if (x < 0) {
        x = x + 32
    }
    return x
}

fun degrees2compasspoint(h: Double): String {
    return compassPoint[cpx(h)]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Index  Compass point         Degree")
        var i: Int = 0
        while (i < headings.size) {
            var h: Double = headings[i]!!
            var idx: BigInteger = ((Math.floorMod(i, 32)) + 1).toBigInteger()
            var cp: String = degrees2compasspoint(h)
            println(((((padLeft(idx.toString(), 4) + "   ") + padRight(cp, 19)) + " ") + format2(h)) + "°")
            i = i + 1
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
