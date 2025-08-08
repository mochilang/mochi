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

var hex_digits: String = "0123456789abcdef"
fun split_by_dot(s: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = 0
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

fun parse_decimal(s: String): Int {
    if (s.length == 0) {
        panic("Invalid IPv4 address format")
    }
    var value: Int = 0
    var i: Int = 0
    while (i < s.length) {
        var c: String = s[i].toString()
        if ((c < "0") || (c > "9")) {
            panic("Invalid IPv4 address format")
        }
        value = (value * 10) + ((c.toInt()))
        i = i + 1
    }
    return value
}

fun to_hex2(n: Int): String {
    var x: Int = n
    var res: String = ""
    while (x > 0) {
        var d: Int = Math.floorMod(x, 16)
        res = hex_digits[d].toString() + res
        x = x / 16
    }
    while (res.length < 2) {
        res = "0" + res
    }
    return res
}

fun ipv4_to_decimal(ipv4_address: String): Int {
    var parts: MutableList<String> = split_by_dot(ipv4_address)
    if (parts.size != 4) {
        panic("Invalid IPv4 address format")
    }
    var result: Int = 0
    var i: Int = 0
    while (i < 4) {
        var oct: Int = parse_decimal(parts[i]!!)
        if ((oct < 0) || (oct > 255)) {
            panic("Invalid IPv4 octet " + oct.toString())
        }
        result = (result * 256) + oct
        i = i + 1
    }
    return result
}

fun alt_ipv4_to_decimal(ipv4_address: String): Int {
    var parts: MutableList<String> = split_by_dot(ipv4_address)
    if (parts.size != 4) {
        panic("Invalid IPv4 address format")
    }
    var hex_str: String = ""
    var i: Int = 0
    while (i < 4) {
        var oct: Int = parse_decimal(parts[i]!!)
        if ((oct < 0) || (oct > 255)) {
            panic("Invalid IPv4 octet " + oct.toString())
        }
        hex_str = hex_str + to_hex2(oct)
        i = i + 1
    }
    var value: Int = 0
    var k: Int = 0
    while (k < hex_str.length) {
        var c: String = hex_str[k].toString()
        var digit: BigInteger = ((0 - 1).toBigInteger())
        var j: Int = 0
        while (j < hex_digits.length) {
            if (hex_digits[j].toString() == c) {
                digit = (j.toBigInteger())
            }
            j = j + 1
        }
        if (digit.compareTo((0).toBigInteger()) < 0) {
            panic("Invalid hex digit")
        }
        value = ((((value * 16)).toBigInteger().add((digit))).toInt())
        k = k + 1
    }
    return value
}

fun decimal_to_ipv4(decimal_ipv4: Int): String {
    if ((decimal_ipv4 < 0) || (decimal_ipv4 > 4294967295L)) {
        panic("Invalid decimal IPv4 address")
    }
    var n: Int = decimal_ipv4
    var parts: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < 4) {
        var octet: Int = Math.floorMod(n, 256)
        parts = run { val _tmp = parts.toMutableList(); _tmp.add(octet.toString()); _tmp }
        n = n / 256
        i = i + 1
    }
    var res: String = ""
    var j: BigInteger = ((parts.size - 1).toBigInteger())
    while (j.compareTo((0).toBigInteger()) >= 0) {
        res = res + parts[(j).toInt()]!!
        if (j.compareTo((0).toBigInteger()) > 0) {
            res = res + "."
        }
        j = j.subtract((1).toBigInteger())
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(ipv4_to_decimal("192.168.0.1"))
        println(ipv4_to_decimal("10.0.0.255"))
        println(alt_ipv4_to_decimal("192.168.0.1"))
        println(alt_ipv4_to_decimal("10.0.0.255"))
        println(decimal_to_ipv4((3232235521L.toInt())))
        println(decimal_to_ipv4(167772415))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
