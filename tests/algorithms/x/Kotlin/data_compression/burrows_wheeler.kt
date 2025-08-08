import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class BWTResult(var bwt_string: String = "", var idx_original_string: Int = 0)
var s: String = "^BANANA"
var result: BWTResult = bwt_transform(s)
fun all_rotations(s: String): MutableList<String> {
    var n: Int = s.length
    var rotations: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < n) {
        var rotation: String = s.substring(i, n) + s.substring(0, i)
        rotations = run { val _tmp = rotations.toMutableList(); _tmp.add(rotation); _tmp }
        i = i + 1
    }
    return rotations
}

fun sort_strings(arr: MutableList<String>): MutableList<String> {
    var n: Int = arr.size
    var i: Int = 1
    while (i < n) {
        var key: String = arr[i]!!
        var j: BigInteger = ((i - 1).toBigInteger())
        while ((j.compareTo((0).toBigInteger()) >= 0) && (arr[(j).toInt()]!! > key)) {
            _listSet(arr, (j.add((1).toBigInteger())).toInt(), arr[(j).toInt()]!!)
            j = j.subtract((1).toBigInteger())
        }
        _listSet(arr, (j.add((1).toBigInteger())).toInt(), key)
        i = i + 1
    }
    return arr
}

fun join_strings(arr: MutableList<String>): String {
    var res: String = ""
    var i: Int = 0
    while (i < arr.size) {
        res = res + arr[i]!!
        i = i + 1
    }
    return res
}

fun bwt_transform(s: String): BWTResult {
    if (s == "") {
        panic("input string must not be empty")
    }
    var rotations: MutableList<String> = all_rotations(s)
    rotations = sort_strings(rotations)
    var last_col: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < rotations.size) {
        var word: String = rotations[i]!!
        last_col = run { val _tmp = last_col.toMutableList(); _tmp.add(word.substring(word.length - 1, word.length)); _tmp }
        i = i + 1
    }
    var bwt_string: String = join_strings(last_col)
    var idx: Int = index_of(rotations, s)
    return BWTResult(bwt_string = bwt_string, idx_original_string = idx)
}

fun index_of(arr: MutableList<String>, target: String): Int {
    var i: Int = 0
    while (i < arr.size) {
        if (arr[i]!! == target) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun reverse_bwt(bwt_string: String, idx_original_string: Int): String {
    if (bwt_string == "") {
        panic("bwt string must not be empty")
    }
    var n: Int = bwt_string.length
    if ((idx_original_string < 0) || (idx_original_string >= n)) {
        panic("index out of range")
    }
    var ordered_rotations: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < n) {
        ordered_rotations = run { val _tmp = ordered_rotations.toMutableList(); _tmp.add(""); _tmp }
        i = i + 1
    }
    var iter: Int = 0
    while (iter < n) {
        var j: Int = 0
        while (j < n) {
            var ch: String = bwt_string.substring(j, j + 1)
            _listSet(ordered_rotations, j, ch + ordered_rotations[j]!!)
            j = j + 1
        }
        ordered_rotations = sort_strings(ordered_rotations)
        iter = iter + 1
    }
    return ordered_rotations[idx_original_string]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(result.bwt_string)
        println(result.idx_original_string)
        println(reverse_bwt(result.bwt_string, result.idx_original_string))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
