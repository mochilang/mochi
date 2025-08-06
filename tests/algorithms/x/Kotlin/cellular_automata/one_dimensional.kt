import java.math.BigInteger

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

var initial: MutableList<Int> = mutableListOf(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
var cells: MutableList<MutableList<Int>> = mutableListOf(initial)
var rules: MutableList<Int> = format_ruleset(30)
var time: Int = 0
var t: Int = 0
fun format_ruleset(ruleset: Int): MutableList<Int> {
    var rs: Int = ruleset
    var bits_rev: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < 8) {
        bits_rev = run { val _tmp = bits_rev.toMutableList(); _tmp.add(Math.floorMod(rs, 2)); _tmp }
        rs = rs / 2
        i = i + 1
    }
    var bits: MutableList<Int> = mutableListOf<Int>()
    var j: BigInteger = ((bits_rev.size - 1).toBigInteger())
    while (j.compareTo((0).toBigInteger()) >= 0) {
        bits = run { val _tmp = bits.toMutableList(); _tmp.add(bits_rev[(j).toInt()]!!); _tmp }
        j = j.subtract((1).toBigInteger())
    }
    return bits
}

fun new_generation(cells: MutableList<MutableList<Int>>, rules: MutableList<Int>, time: Int): MutableList<Int> {
    var population: Int = (cells[0]!!).size
    var next_generation: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < population) {
        var left_neighbor: Int = (if (i == 0) 0 else (((cells[time]!!) as MutableList<Int>))[i - 1]!! as Int)
        var right_neighbor: Int = (if (i == (population - 1)) 0 else (((cells[time]!!) as MutableList<Int>))[i + 1]!! as Int)
        var center: Int = (((cells[time]!!) as MutableList<Int>))[i]!!
        var idx: Int = 7 - (((left_neighbor * 4) + (center * 2)) + right_neighbor)
        next_generation = run { val _tmp = next_generation.toMutableList(); _tmp.add(rules[idx]!!); _tmp }
        i = i + 1
    }
    return next_generation
}

fun cells_to_string(row: MutableList<Int>): String {
    var result: String = ""
    var i: Int = 0
    while (i < row.size) {
        if (row[i]!! == 1) {
            result = result + "#"
        } else {
            result = result + "."
        }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (time < 16) {
            var next: MutableList<Int> = new_generation(cells, rules, time)
            cells = run { val _tmp = cells.toMutableList(); _tmp.add(next); _tmp }
            time = time + 1
        }
        while (t < cells.size) {
            println(cells_to_string(cells[t]!!))
            t = t + 1
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
