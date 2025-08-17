fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

fun exact_prime_factor_count(n: Int): Int {
    var count: Int = (0).toInt()
    var num: Int = (n).toInt()
    if ((Math.floorMod(num, 2)) == 0) {
        count = count + 1
        while ((Math.floorMod(num, 2)) == 0) {
            num = num / 2
        }
    }
    var i: Int = (3).toInt()
    while (((i).toLong() * (i).toLong()) <= num) {
        if ((Math.floorMod(num, i)) == 0) {
            count = count + 1
            while ((Math.floorMod(num, i)) == 0) {
                num = num / i
            }
        }
        i = i + 2
    }
    if (num > 2) {
        count = count + 1
    }
    return count
}

fun ln(x: Double): Double {
    var ln2: Double = 0.6931471805599453
    var y: Double = x
    var k: Double = 0.0
    while (y > 2.0) {
        y = y / 2.0
        k = k + ln2
    }
    while (y < 1.0) {
        y = y * 2.0
        k = k - ln2
    }
    var t: Double = (y - 1.0) / (y + 1.0)
    var term: Double = t
    var sum: Double = 0.0
    var n: Int = (1).toInt()
    while (n <= 19) {
        sum = sum + (term / (n.toDouble()))
        term = (term * t) * t
        n = n + 2
    }
    return k + (2.0 * sum)
}

fun floor(x: Double): Double {
    var i: Int = (x.toInt()).toInt()
    if ((i.toDouble()) > x) {
        i = i - 1
    }
    return i.toDouble()
}

fun round4(x: Double): Double {
    var m: Double = 10000.0
    return floor((x * m) + 0.5) / m
}

fun user_main(): Unit {
    var n: Int = (51242183).toInt()
    var count: Int = (exact_prime_factor_count(n)).toInt()
    println("The number of distinct prime factors is/are " + _numToStr(count))
    var loglog: Double = ln(ln(n.toDouble()))
    println("The value of log(log(n)) is " + _numToStr(round4(loglog)))
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
