import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/spoj/x/mochi"

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var primes: MutableList<Int> = precompute(32000)
var t: Int = ((input()).toBigInteger().toInt()).toInt()
var case_idx: Int = (0).toInt()
fun split(s: String, sep: String): MutableList<String> {
    var parts: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        if ((((sep.length > 0) && ((i + sep.length) <= s.length) as Boolean)) && (s.substring(i, i + sep.length) == sep)) {
            parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
            cur = ""
            i = (i + sep.length).toInt()
        } else {
            cur = cur + _sliceStr(s, i, i + 1)
            i = (i + 1).toInt()
        }
    }
    parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
    return parts
}

fun precompute(limit: Int): MutableList<Int> {
    var sieve: MutableList<Boolean> = mutableListOf<Boolean>()
    for (i in 0 until limit + 1) {
        sieve = run { val _tmp = sieve.toMutableList(); _tmp.add(true); _tmp }
    }
    _listSet(sieve, 0, false)
    _listSet(sieve, 1, false)
    var p: Int = (2).toInt()
    while ((p * p) <= limit) {
        if ((sieve[p]!!) as Boolean) {
            var j: Int = (p * p).toInt()
            while (j <= limit) {
                _listSet(sieve, j, false)
                j = (j + p).toInt()
            }
        }
        p = p + 1
    }
    var primes: MutableList<Int> = mutableListOf<Int>()
    for (i in 2 until limit + 1) {
        if ((sieve[i]!!) as Boolean) {
            primes = run { val _tmp = primes.toMutableList(); _tmp.add(i); _tmp }
        }
    }
    return primes
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (case_idx < t) {
            var line: String = input()
            var parts: MutableList<String> = split(line, " ")
            var m: Int = ((parts[0]!!).toBigInteger().toInt()).toInt()
            var n: Int = ((parts[1]!!).toBigInteger().toInt()).toInt()
            var size: Int = ((n - m) + 1).toInt()
            var segment: MutableList<Boolean> = mutableListOf<Boolean>()
            for (i in 0 until size) {
                segment = run { val _tmp = segment.toMutableList(); _tmp.add(true); _tmp }
            }
            for (p in primes) {
                if ((p * p) > n) {
                    break
                }
                var start: Int = (p * p).toInt()
                if (start < m) {
                    var rem: Int = (Math.floorMod(m, p)).toInt()
                    if (rem == 0) {
                        start = (m).toInt()
                    } else {
                        start = (m + (p - rem)).toInt()
                    }
                }
                var j: Int = (start).toInt()
                while (j <= n) {
                    _listSet(segment, j - m, false)
                    j = (j + p).toInt()
                }
            }
            if (m == 1) {
                _listSet(segment, 0, false)
            }
            var i: Int = (0).toInt()
            while (i < size) {
                if ((segment[i]!!) as Boolean) {
                    println(i + m)
                }
                i = (i + 1).toInt()
            }
            if (case_idx < (t - 1)) {
                println("")
            }
            case_idx = (case_idx + 1).toInt()
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
