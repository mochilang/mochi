import java.math.BigInteger

fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
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

var seq2: MutableList<Int> = gray_code(2)
fun gray_code(bit_count: Int): MutableList<Int> {
    if (bit_count == 0) {
        return mutableListOf(0)
    }
    var prev: MutableList<Int> = gray_code(bit_count - 1)
    var add_val: Int = ((pow2(bit_count - 1)).toInt())
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < prev.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(prev[i]!!); _tmp }
        i = i + 1
    }
    var j: BigInteger = ((prev.size - 1).toBigInteger())
    while (j.compareTo((0).toBigInteger()) >= 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(prev[(j).toInt()]!! + add_val); _tmp }
        j = j.subtract((1).toBigInteger())
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(seq2.toString())
        var seq1: MutableList<Int> = gray_code(1)
        println(seq1.toString())
        var seq3: MutableList<Int> = gray_code(3)
        println(seq3.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
