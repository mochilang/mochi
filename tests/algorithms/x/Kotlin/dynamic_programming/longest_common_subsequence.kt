fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class LcsResult(var length: Int = 0, var sequence: String = "")
var a: String = "AGGTAB"
var b: String = "GXTXAYB"
var res: LcsResult = longest_common_subsequence(a, b)
fun zeros_matrix(rows: Int, cols: Int): MutableList<MutableList<Int>> {
    var matrix: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i <= rows) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j <= cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        matrix = run { val _tmp = matrix.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return matrix
}

fun longest_common_subsequence(x: String, y: String): LcsResult {
    var m: Int = (x.length).toInt()
    var n: Int = (y.length).toInt()
    var dp: MutableList<MutableList<Int>> = zeros_matrix(m, n)
    var i: Int = (1).toInt()
    while (i <= m) {
        var j: Int = (1).toInt()
        while (j <= n) {
            if (x[i - 1].toString() == y[j - 1].toString()) {
                _listSet(dp[i]!!, j, (((dp[i - 1]!!) as MutableList<Int>))[j - 1]!! + 1)
            } else {
                if ((((dp[i - 1]!!) as MutableList<Int>))[j]!! > (((dp[i]!!) as MutableList<Int>))[j - 1]!!) {
                    _listSet(dp[i]!!, j, (((dp[i - 1]!!) as MutableList<Int>))[j]!!)
                } else {
                    _listSet(dp[i]!!, j, (((dp[i]!!) as MutableList<Int>))[j - 1]!!)
                }
            }
            j = j + 1
        }
        i = i + 1
    }
    var seq: String = ""
    var i2: Int = (m).toInt()
    var j2: Int = (n).toInt()
    while ((i2 > 0) && (j2 > 0)) {
        if (x[i2 - 1].toString() == y[j2 - 1].toString()) {
            seq = x[i2 - 1].toString() + seq
            i2 = i2 - 1
            j2 = j2 - 1
        } else {
            if ((((dp[i2 - 1]!!) as MutableList<Int>))[j2]!! >= (((dp[i2]!!) as MutableList<Int>))[j2 - 1]!!) {
                i2 = i2 - 1
            } else {
                j2 = j2 - 1
            }
        }
    }
    return LcsResult(length = (((dp[m]!!) as MutableList<Int>))[n]!!, sequence = seq)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((("len = " + res.length.toString()) + ", sub-sequence = ") + res.sequence)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
