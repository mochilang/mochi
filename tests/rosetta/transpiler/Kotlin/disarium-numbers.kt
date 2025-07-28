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

fun pow(base: Int, exp: Int): Int {
    var result: Int = 1
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun isDisarium(n: Int): Boolean {
    var digits: MutableList<Int> = mutableListOf<Int>()
    var x: Int = n
    if (x == 0) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
    }
    while (x > 0) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(Math.floorMod(x, 10)); _tmp } as MutableList<Int>
        x = (x / 10).toInt()
    }
    var sum: Int = 0
    var pos: Int = 1
    var i: BigInteger = (digits.size - 1).toBigInteger()
    while (i.compareTo(0.toBigInteger()) >= 0) {
        sum = sum + pow(digits[(i).toInt()], pos)
        pos = pos + 1
        i = i.subtract(1.toBigInteger())
    }
    return sum == n
}

fun user_main(): Unit {
    var count: Int = 0
    var n: Int = 0
    while ((count < 19) && (n < 3000000)) {
        if ((isDisarium(n)) as Boolean) {
            println(n.toString())
            count = count + 1
        }
        n = n + 1
    }
    println(("\nFound the first " + count.toString()) + " Disarium numbers.")
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
