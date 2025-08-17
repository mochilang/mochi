import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
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

fun parse_decimal(s: String): Int {
    var value: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        if ((c < "0") || (c > "9")) {
            panic("invalid literal")
        }
        value = (value * 10) + (c.toBigInteger().toInt())
        i = i + 1
    }
    return value
}

fun zeller_day(date_input: String): String {
    var days: MutableMap<Int, String> = mutableMapOf<Int, String>(0 to ("Sunday"), 1 to ("Monday"), 2 to ("Tuesday"), 3 to ("Wednesday"), 4 to ("Thursday"), 5 to ("Friday"), 6 to ("Saturday")) as MutableMap<Int, String>
    if (date_input.length != 10) {
        panic("Must be 10 characters long")
    }
    var m: Int = (parse_decimal(_sliceStr(date_input, 0, 2))).toInt()
    if ((m <= 0) || (m >= 13)) {
        panic("Month must be between 1 - 12")
    }
    var sep1: String = date_input[2].toString()
    if ((sep1 != "-") && (sep1 != "/")) {
        panic("Date separator must be '-' or '/'")
    }
    var d: Int = (parse_decimal(_sliceStr(date_input, 3, 5))).toInt()
    if ((d <= 0) || (d >= 32)) {
        panic("Date must be between 1 - 31")
    }
    var sep2: String = date_input[5].toString()
    if ((sep2 != "-") && (sep2 != "/")) {
        panic("Date separator must be '-' or '/'")
    }
    var y: Int = (parse_decimal(_sliceStr(date_input, 6, 10))).toInt()
    if ((y <= 45) || (y >= 8500)) {
        panic("Year out of range. There has to be some sort of limit...right?")
    }
    var year: Int = (y).toInt()
    var month: Int = (m).toInt()
    if (month <= 2) {
        year = year - 1
        month = month + 12
    }
    var c: Int = (year / 100).toInt()
    var k: Int = (Math.floorMod(year, 100)).toInt()
    var t: Int = (((2.6 * (month.toDouble())) - 5.39).toInt()).toInt()
    var u: Int = (c / 4).toInt()
    var v: Int = (k / 4).toInt()
    var x: Int = (d + k).toInt()
    var z: Int = (((t + u) + v) + x).toInt()
    var w: Int = (z - (2 * c)).toInt()
    var f: Int = (Math.floorMod(w, 7)).toInt()
    if (f < 0) {
        f = f + 7
    }
    return (days)[f] as String
}

fun zeller(date_input: String): String {
    var day: String = zeller_day(date_input)
    return ((("Your date " + date_input) + ", is a ") + day) + "!"
}

fun test_zeller(): Unit {
    var inputs: MutableList<String> = mutableListOf("01-31-2010", "02-01-2010", "11-26-2024", "07-04-1776")
    var expected: MutableList<String> = mutableListOf("Sunday", "Monday", "Tuesday", "Thursday")
    var i: Int = (0).toInt()
    while (i < inputs.size) {
        var res: String = zeller_day(inputs[i]!!)
        if (res != expected[i]!!) {
            panic("zeller test failed")
        }
        i = i + 1
    }
}

fun user_main(): Unit {
    test_zeller()
    println(zeller("01-31-2010"))
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
