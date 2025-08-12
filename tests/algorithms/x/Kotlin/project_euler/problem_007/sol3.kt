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

fun is_prime(number: Int): Boolean {
    if ((1 < number) && (number < 4)) {
        return true
    } else {
        if ((((number < 2) || ((Math.floorMod(number, 2)) == 0) as Boolean)) || ((Math.floorMod(number, 3)) == 0)) {
            return false
        }
    }
    var i: Int = (5).toInt()
    while ((i * i) <= number) {
        if (((Math.floorMod(number, i)) == 0) || ((Math.floorMod(number, (i + 2))) == 0)) {
            return false
        }
        i = i + 6
    }
    return true
}

fun solution(nth: Int): Int {
    var count: Int = (0).toInt()
    var num: Int = (2).toInt()
    while (true) {
        if (((is_prime(num)) as Boolean)) {
            count = count + 1
            if (count == nth) {
                return num
            }
        }
        num = num + 1
    }
    return 0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("solution() = " + solution(10001).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
