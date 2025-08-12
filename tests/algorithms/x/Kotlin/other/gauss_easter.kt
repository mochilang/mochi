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

data class EasterDate(var month: Int = 0, var day: Int = 0)
var years: MutableList<Int> = mutableListOf(1994, 2000, 2010, 2021, 2023, 2032, 2100)
var i: Int = (0).toInt()
fun gauss_easter(year: Int): EasterDate {
    var metonic_cycle: Int = (Math.floorMod(year, 19)).toInt()
    var julian_leap_year: Int = (Math.floorMod(year, 4)).toInt()
    var non_leap_year: Int = (Math.floorMod(year, 7)).toInt()
    var leap_day_inhibits: Int = (year / 100).toInt()
    var lunar_orbit_correction: Int = ((13 + (8 * leap_day_inhibits)) / 25).toInt()
    var leap_day_reinstall_number: Double = ((leap_day_inhibits.toDouble())) / 4.0
    var secular_moon_shift: Double = Math.floorMod((((15.0 - ((lunar_orbit_correction.toDouble()))) + ((leap_day_inhibits.toDouble()))) - leap_day_reinstall_number), 30.0)
    var century_starting_point: Double = Math.floorMod(((4.0 + ((leap_day_inhibits.toDouble()))) - leap_day_reinstall_number), 7.0)
    var days_to_add: Double = Math.floorMod(((19.0 * ((metonic_cycle.toDouble()))) + secular_moon_shift), 30.0)
    var days_from_phm_to_sunday: Double = Math.floorMod(((((2.0 * ((julian_leap_year.toDouble()))) + (4.0 * ((non_leap_year.toDouble())))) + (6.0 * days_to_add)) + century_starting_point), 7.0)
    if ((days_to_add == 29.0) && (days_from_phm_to_sunday == 6.0)) {
        return EasterDate(month = 4, day = 19)
    }
    if ((days_to_add == 28.0) && (days_from_phm_to_sunday == 6.0)) {
        return EasterDate(month = 4, day = 18)
    }
    var offset: Int = (((days_to_add + days_from_phm_to_sunday).toInt())).toInt()
    var total: Int = (22 + offset).toInt()
    if (total > 31) {
        return EasterDate(month = 4, day = total - 31)
    }
    return EasterDate(month = 3, day = total)
}

fun format_date(year: Int, d: EasterDate): String {
    var month: String = (if (d.month < 10) "0" + d.month.toString() else d.month.toString() as String)
    var day: String = (if (d.day < 10) "0" + d.day.toString() else d.day.toString() as String)
    return (((year.toString() + "-") + month) + "-") + day
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < years.size) {
            var y: Int = (years[i]!!).toInt()
            var e: EasterDate = gauss_easter(y)
            println((("Easter in " + y.toString()) + " is ") + format_date(y, e))
            i = (i + 1).toInt()
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
