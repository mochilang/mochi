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

fun nor_gate(input_1: Int, input_2: Int): Int {
    if ((input_1 == 0) && (input_2 == 0)) {
        return 1
    }
    return 0
}

fun center(s: String, width: Int): String {
    var total: BigInteger = ((width - s.length).toBigInteger())
    if (total.compareTo((0).toBigInteger()) <= 0) {
        return s
    }
    var left: BigInteger = total.divide((2).toBigInteger())
    var right: BigInteger = total.subtract((left))
    var res: String = s
    var i: Int = 0
    while ((i).toBigInteger().compareTo((left)) < 0) {
        res = " " + res
        i = i + 1
    }
    var j: Int = 0
    while ((j).toBigInteger().compareTo((right)) < 0) {
        res = res + " "
        j = j + 1
    }
    return res
}

fun make_table_row(i: Int, j: Int): String {
    var output: Int = nor_gate(i, j)
    return ((((("| " + center(i.toString(), 8)) + " | ") + center(j.toString(), 8)) + " | ") + center(output.toString(), 8)) + " |"
}

fun truth_table(): String {
    return ((((((("Truth Table of NOR Gate:\n" + "| Input 1 | Input 2 | Output  |\n") + make_table_row(0, 0)) + "\n") + make_table_row(0, 1)) + "\n") + make_table_row(1, 0)) + "\n") + make_table_row(1, 1)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(nor_gate(0, 0))
        println(nor_gate(0, 1))
        println(nor_gate(1, 0))
        println(nor_gate(1, 1))
        println(truth_table())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
