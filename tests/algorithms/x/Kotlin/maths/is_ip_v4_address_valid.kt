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

fun split_by_dot(s: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        if (c == ".") {
            res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
            current = ""
        } else {
            current = current + c
        }
        i = i + 1
    }
    res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
    return res
}

fun is_digit_str(s: String): Boolean {
    if (s.length == 0) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        if ((c < "0") || (c > "9")) {
            return false
        }
        i = i + 1
    }
    return true
}

fun parse_decimal(s: String): Int {
    var value: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        value = (((value).toLong() * (10).toLong()) + (c.toBigInteger().toInt()).toLong()).toInt()
        i = i + 1
    }
    return value
}

fun is_ip_v4_address_valid(ip: String): Boolean {
    var octets: MutableList<String> = split_by_dot(ip)
    if (octets.size != 4) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < 4) {
        var oct: String = octets[i]!!
        if (!is_digit_str(oct)) {
            return false
        }
        var number: Int = (parse_decimal(oct)).toInt()
        if (_numToStr(number).length != oct.length) {
            return false
        }
        if ((number < 0) || (number > 255)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(is_ip_v4_address_valid("192.168.0.23").toString())
        println(is_ip_v4_address_valid("192.256.15.8").toString())
        println(is_ip_v4_address_valid("172.100.0.8").toString())
        println(is_ip_v4_address_valid("255.256.0.256").toString())
        println(is_ip_v4_address_valid("1.2.33333333.4").toString())
        println(is_ip_v4_address_valid("1.2.-3.4").toString())
        println(is_ip_v4_address_valid("1.2.3").toString())
        println(is_ip_v4_address_valid("1.2.3.4.5").toString())
        println(is_ip_v4_address_valid("1.2.A.4").toString())
        println(is_ip_v4_address_valid("0.0.0.0").toString())
        println(is_ip_v4_address_valid("1.2.3.").toString())
        println(is_ip_v4_address_valid("1.2.3.05").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
