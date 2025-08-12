import java.math.BigInteger

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

fun allocation_num(number_of_bytes: Int, partitions: Int): MutableList<String> {
    if (partitions <= 0) {
        panic("partitions must be a positive number!")
    }
    if (partitions > number_of_bytes) {
        panic("partitions can not > number_of_bytes!")
    }
    var bytes_per_partition: Int = (number_of_bytes / partitions).toInt()
    var allocation_list: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < partitions) {
        var start_bytes: Int = ((i * bytes_per_partition) + 1).toInt()
        var end_bytes: Int = (if (i == (partitions - 1)) number_of_bytes else (i + 1) * bytes_per_partition).toInt()
        allocation_list = run { val _tmp = allocation_list.toMutableList(); _tmp.add((start_bytes.toString() + "-") + end_bytes.toString()); _tmp }
        i = i + 1
    }
    return allocation_list
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(allocation_num(16647, 4).toString())
        println(allocation_num(50000, 5).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
