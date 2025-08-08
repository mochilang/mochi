import java.math.BigInteger

fun pow2(n: Int): Int {
var v = 1
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var st1: MutableList<MutableList<Int>> = build_sparse_table(mutableListOf(8, 1, 0, 3, 4, 9, 3))
fun int_log2(n: Int): Int {
    var v: Int = (n).toInt()
    var res: Int = (0).toInt()
    while (v > 1) {
        v = v / 2
        res = res + 1
    }
    return res
}

fun build_sparse_table(number_list: MutableList<Int>): MutableList<MutableList<Int>> {
    if (number_list.size == 0) {
        panic("empty number list not allowed")
    }
    var length: Int = (number_list.size).toInt()
    var row: Int = (int_log2(length) + 1).toInt()
    var sparse_table: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var j: Int = (0).toInt()
    while (j < row) {
        var inner: MutableList<Int> = mutableListOf<Int>()
        var i: Int = (0).toInt()
        while (i < length) {
            inner = run { val _tmp = inner.toMutableList(); _tmp.add(0); _tmp }
            i = i + 1
        }
        sparse_table = run { val _tmp = sparse_table.toMutableList(); _tmp.add(inner); _tmp }
        j = j + 1
    }
    var i: Int = (0).toInt()
    while (i < length) {
        _listSet(sparse_table[0]!!, i, number_list[i]!!)
        i = i + 1
    }
    j = 1
    while (pow2(j) <= length) {
        i = 0
        while (((i + pow2(j)) - 1) < length) {
            var left: Int = ((((sparse_table[j - 1]!!) as MutableList<Int>))[i + pow2(j - 1)]!!).toInt()
            var right: Int = ((((sparse_table[j - 1]!!) as MutableList<Int>))[i]!!).toInt()
            if (left < right) {
                _listSet(sparse_table[j]!!, i, left)
            } else {
                _listSet(sparse_table[j]!!, i, right)
            }
            i = i + 1
        }
        j = j + 1
    }
    return sparse_table
}

fun query(sparse_table: MutableList<MutableList<Int>>, left_bound: Int, right_bound: Int): Int {
    if ((left_bound < 0) || (right_bound >= (sparse_table[0]!!).size)) {
        panic("list index out of range")
    }
    var interval: Int = ((right_bound - left_bound) + 1).toInt()
    var j: Int = (int_log2(interval)).toInt()
    var val1: Int = ((((sparse_table[j]!!) as MutableList<Int>))[(right_bound - pow2(j)) + 1]!!).toInt()
    var val2: Int = ((((sparse_table[j]!!) as MutableList<Int>))[left_bound]!!).toInt()
    if (val1 < val2) {
        return val1
    }
    return val2
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(st1.toString())
        var st2: MutableList<MutableList<Int>> = build_sparse_table(mutableListOf(3, 1, 9))
        println(st2.toString())
        println(query(st1, 0, 4).toString())
        println(query(st1, 4, 6).toString())
        println(query(st2, 2, 2).toString())
        println(query(st2, 0, 1).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
