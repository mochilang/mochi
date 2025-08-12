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

fun pow_int(base: Int, exp: Int): Int {
    var result: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun prime_factors(n: Int): MutableList<Int> {
    if (n <= 0) {
        panic("Only positive integers have prime factors")
    }
    var num: Int = (n).toInt()
    var pf: MutableList<Int> = mutableListOf<Int>()
    while ((Math.floorMod(num, 2)) == 0) {
        pf = run { val _tmp = pf.toMutableList(); _tmp.add(2); _tmp }
        num = num / 2
    }
    var i: Int = (3).toInt()
    while ((i * i) <= num) {
        while ((Math.floorMod(num, i)) == 0) {
            pf = run { val _tmp = pf.toMutableList(); _tmp.add(i); _tmp }
            num = num / i
        }
        i = i + 2
    }
    if (num > 2) {
        pf = run { val _tmp = pf.toMutableList(); _tmp.add(num); _tmp }
    }
    return pf
}

fun number_of_divisors(n: Int): Int {
    if (n <= 0) {
        panic("Only positive numbers are accepted")
    }
    var num: Int = (n).toInt()
    var div: Int = (1).toInt()
    var temp: Int = (1).toInt()
    while ((Math.floorMod(num, 2)) == 0) {
        temp = temp + 1
        num = num / 2
    }
    div = div * temp
    var i: Int = (3).toInt()
    while ((i * i) <= num) {
        temp = 1
        while ((Math.floorMod(num, i)) == 0) {
            temp = temp + 1
            num = num / i
        }
        div = div * temp
        i = i + 2
    }
    if (num > 1) {
        div = div * 2
    }
    return div
}

fun sum_of_divisors(n: Int): Int {
    if (n <= 0) {
        panic("Only positive numbers are accepted")
    }
    var num: Int = (n).toInt()
    var s: Int = (1).toInt()
    var temp: Int = (1).toInt()
    while ((Math.floorMod(num, 2)) == 0) {
        temp = temp + 1
        num = num / 2
    }
    if (temp > 1) {
        s = s * ((pow_int(2, temp) - 1) / (2 - 1))
    }
    var i: Int = (3).toInt()
    while ((i * i) <= num) {
        temp = 1
        while ((Math.floorMod(num, i)) == 0) {
            temp = temp + 1
            num = num / i
        }
        if (temp > 1) {
            s = s * ((pow_int(i, temp) - 1) / (i - 1))
        }
        i = i + 2
    }
    return s
}

fun contains(arr: MutableList<Int>, x: Int): Boolean {
    var idx: Int = (0).toInt()
    while (idx < arr.size) {
        if (arr[idx]!! == x) {
            return true
        }
        idx = idx + 1
    }
    return false
}

fun unique(arr: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var idx: Int = (0).toInt()
    while (idx < arr.size) {
        var v: Int = (arr[idx]!!).toInt()
        if (!((result.contains(v)) as Boolean)) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(v); _tmp }
        }
        idx = idx + 1
    }
    return result
}

fun euler_phi(n: Int): Int {
    if (n <= 0) {
        panic("Only positive numbers are accepted")
    }
    var s: Int = (n).toInt()
    var factors: MutableList<Int> = unique(prime_factors(n))
    var idx: Int = (0).toInt()
    while (idx < factors.size) {
        var x: Int = (factors[idx]!!).toInt()
        s = (s / x) * (x - 1)
        idx = idx + 1
    }
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(prime_factors(100).toString())
        println(number_of_divisors(100).toString())
        println(sum_of_divisors(100).toString())
        println(euler_phi(100).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
