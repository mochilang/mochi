import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var DOOMSDAY_LEAP: MutableList<Int> = mutableListOf(4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5)
var DOOMSDAY_NOT_LEAP: MutableList<Int> = mutableListOf(3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5)
var WEEK_DAY_NAMES: MutableMap<Int, String> = mutableMapOf<Int, String>(0 to ("Sunday"), 1 to ("Monday"), 2 to ("Tuesday"), 3 to ("Wednesday"), 4 to ("Thursday"), 5 to ("Friday"), 6 to ("Saturday"))
fun get_week_day(year: Int, month: Int, day: Int): String {
    if (year < 100) {
        panic("year should be in YYYY format")
    }
    if ((month < 1) || (month > 12)) {
        panic("month should be between 1 to 12")
    }
    if ((day < 1) || (day > 31)) {
        panic("day should be between 1 to 31")
    }
    var century: Int = (year / 100).toInt()
    var century_anchor: Int = (Math.floorMod(((5 * (Math.floorMod(century, 4))) + 2), 7)).toInt()
    var centurian: Int = (Math.floorMod(year, 100)).toInt()
    var centurian_m: Int = (Math.floorMod(centurian, 12)).toInt()
    var dooms_day: Int = (Math.floorMod(((((centurian / 12) + centurian_m) + (centurian_m / 4)) + century_anchor), 7)).toInt()
    var day_anchor: Int = (if (((Math.floorMod(year, 4)) != 0) || (((centurian == 0) && ((Math.floorMod(year, 400)) != 0) as Boolean))) DOOMSDAY_NOT_LEAP[month - 1]!! else DOOMSDAY_LEAP[month - 1]!!).toInt()
    var week_day: BigInteger = ((Math.floorMod(((dooms_day + day) - day_anchor), 7)).toBigInteger())
    if (week_day.compareTo((0).toBigInteger()) < 0) {
        week_day = week_day.add((7).toBigInteger())
    }
    return (WEEK_DAY_NAMES)[(week_day).toInt()] as String
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(get_week_day(2020, 10, 24))
        println(get_week_day(2017, 10, 24))
        println(get_week_day(2019, 5, 3))
        println(get_week_day(1970, 9, 16))
        println(get_week_day(1870, 8, 13))
        println(get_week_day(2040, 3, 14))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
