import java.math.BigInteger

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

fun solution(length: Int): Int {
    var ways: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i <= length) {
        var row: MutableList<Int> = mutableListOf<Int>()
        row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
        row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
        row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
        ways = run { val _tmp = ways.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var row_length: Int = (0).toInt()
    while (row_length <= length) {
        var tile_length: Int = (2).toInt()
        while (tile_length <= 4) {
            var tile_start: Int = (0).toInt()
            while (tile_start <= (row_length - tile_length)) {
                var remaining: Int = ((row_length - tile_start) - tile_length).toInt()
                _listSet(ways[row_length]!!, tile_length - 2, ((((ways[row_length]!!) as MutableList<Int>))[tile_length - 2]!! + (((ways[remaining]!!) as MutableList<Int>))[tile_length - 2]!!) + 1)
                tile_start = tile_start + 1
            }
            tile_length = tile_length + 1
        }
        row_length = row_length + 1
    }
    var total: Int = (0).toInt()
    var j: Int = (0).toInt()
    while (j < 3) {
        total = total + (((ways[length]!!) as MutableList<Int>))[j]!!
        j = j + 1
    }
    return total
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(solution(5))
        println(solution(50))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
