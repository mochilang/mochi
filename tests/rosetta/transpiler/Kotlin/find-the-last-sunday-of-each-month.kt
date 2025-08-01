import java.math.BigInteger

fun input(): String = readLine() ?: ""

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

fun leapYear(y: Int): Boolean {
    return (((((Math.floorMod(y, 4)) == 0) && ((Math.floorMod(y, 100)) != 0) as Boolean)) || ((Math.floorMod(y, 400)) == 0)) as Boolean
}

fun monthDays(y: Int, m: Int): Int {
    var days: MutableList<Int> = mutableListOf(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    if ((m == 2) && leapYear(y)) {
        return 29
    }
    return days[m]!!
}

fun zeller(y: Int, m: Int, d: Int): Int {
    var mm: Int = m
    var yy: Int = y
    if (mm < 3) {
        mm = mm + 12
        yy = yy - 1
    }
    var K: BigInteger = (Math.floorMod(yy, 100)).toBigInteger()
    var J: Int = yy / 100
    var h: BigInteger = ((((((d + ((13 * (mm + 1)) / 5))).toBigInteger().add(K)).add((K.divide(4.toBigInteger())))).add((J / 4).toBigInteger())).add((5 * J).toBigInteger())).remainder(7.toBigInteger())
    return ((h.add(6.toBigInteger())).remainder(7.toBigInteger())).toInt()
}

fun lastSunday(y: Int, m: Int): Int {
    var day: Int = monthDays(y, m)
    while ((day > 0) && (zeller(y, m, day) != 0)) {
        day = day - 1
    }
    return day
}

fun monthName(m: Int): String {
    var names: MutableList<String> = mutableListOf("", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    return names[m]!!
}

fun user_main(): Unit {
    var year: Int = (input()).toInt()
    println("Last Sundays of each month of " + year.toString())
    println("==================================")
    var m: Int = 1
    while (m <= 12) {
        var day: Int = lastSunday(year, m)
        println((monthName(m) + ": ") + day.toString())
        m = m + 1
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
