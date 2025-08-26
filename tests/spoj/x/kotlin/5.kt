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

fun next_pal(s: String): String {
    var digitMap: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    var n: Int = (s.length).toInt()
    var num: MutableList<Int> = mutableListOf<Int>()
    for (i in 0 until n) {
        num = run { val _tmp = num.toMutableList(); _tmp.add(((digitMap)[_sliceStr(s, i, i + 1)] as Int).toInt()); _tmp }
    }
    var all9: Boolean = true
    for (d in num) {
        if ((d as Int) != 9) {
            all9 = false
            break
        }
    }
    if (all9 as Boolean) {
        var res: String = "1"
        for (_u1 in 0 until n - 1) {
            res = res + "0"
        }
        res = res + "1"
        return res
    }
    var left: Int = ((Math.floorDiv(n, 2)) - 1).toInt()
    var right: Int = (if ((Math.floorMod(n, 2)) == 0) Math.floorDiv(n, 2) else (Math.floorDiv(n, 2)) + 1.toInt()).toInt()
    while ((((left >= 0) && (right < n) as Boolean)) && (num[left]!! == num[right]!!)) {
        left = left - 1
        right = right + 1
    }
    var smaller = (left < 0) || (num[left]!! < num[right]!!)
    left = (Math.floorDiv(n, 2)) - 1
    right = if ((Math.floorMod(n, 2)) == 0) Math.floorDiv(n, 2) else (Math.floorDiv(n, 2)) + 1.toInt()
    while (left >= 0) {
        _listSet(num, right, num[left]!!)
        left = left - 1
        right = right + 1
    }
    if (smaller as Boolean) {
        var carry: Int = (1).toInt()
        left = (Math.floorDiv(n, 2)) - 1
        if ((Math.floorMod(n, 2)) == 1) {
            var mid: Int = (Math.floorDiv(n, 2)).toInt()
            _listSet(num, mid, num[mid]!! + carry)
            carry = Math.floorDiv(num[mid]!!, 10)
            _listSet(num, mid, Math.floorMod(num[mid]!!, 10))
            right = mid + 1
        } else {
            right = Math.floorDiv(n, 2)
        }
        while (left >= 0) {
            _listSet(num, left, num[left]!! + carry)
            carry = Math.floorDiv(num[left]!!, 10)
            _listSet(num, left, Math.floorMod(num[left]!!, 10))
            _listSet(num, right, num[left]!!)
            left = left - 1
            right = right + 1
        }
    }
    var out: String = ""
    for (d in num) {
        out = out + d.toString()
    }
    return out
}

fun parseIntStr(str: String): Int {
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    var i: Int = (0).toInt()
    var n: Int = (0).toInt()
    while (i < str.length) {
        n = (n * 10) + (((digits)[_sliceStr(str, i, i + 1)] as Int).toInt())
        i = i + 1
    }
    return n
}

fun user_main(): Unit {
    var tStr: String = input()
    if (tStr == "") {
        return
    }
    var t: Int = (parseIntStr(tStr)).toInt()
    for (_u2 in 0 until t) {
        var s: String = input()
        println(next_pal(s))
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
