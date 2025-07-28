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

val days: MutableList<String> = mutableListOf("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
val firstDaysCommon: MutableList<Int> = mutableListOf(3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5)
val firstDaysLeap: MutableList<Int> = mutableListOf(4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5)
fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    val digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + (digits)[str.substring(i, i + 1)] as Int
        i = i + 1
    }
    if (neg as Boolean) {
        n = 0 - n
    }
    return n
}

fun anchorDay(y: Int): Int {
    return Math.floorMod((((2 + (5 * (Math.floorMod(y, 4)))) + (4 * (Math.floorMod(y, 100)))) + (6 * (Math.floorMod(y, 400)))), 7)
}

fun isLeapYear(y: Int): Boolean {
    return (((Math.floorMod(y, 4)) == 0) && ((((Math.floorMod(y, 100)) != 0) || ((Math.floorMod(y, 400)) == 0) as Boolean))) as Boolean
}

fun user_main(): Unit {
    val dates: MutableList<String> = mutableListOf("1800-01-06", "1875-03-29", "1915-12-07", "1970-12-23", "2043-05-14", "2077-02-12", "2101-04-02")
    println("Days of week given by Doomsday rule:")
    for (date in dates) {
        val y: Int = parseIntStr(date.substring(0, 4))
        val m: Int = parseIntStr(date.substring(5, 7)) - 1
        val d: Int = parseIntStr(date.substring(8, 10))
        val a: Int = anchorDay(y.toInt())
        var f: Int = firstDaysCommon[m]
        if ((isLeapYear(y.toInt())) as Boolean) {
            f = firstDaysLeap[m]
        }
        var w: Int = d - f
        if (w < 0) {
            w = 7 + w
        }
        val dow: BigInteger = (Math.floorMod((a + w), 7)).toBigInteger()
        println((date + " -> ") + days[(dow).toInt()])
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
