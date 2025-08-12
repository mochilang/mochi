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

sealed class Item
data class Int(var value: Int) : Item()
data class Str(var value: String) : Item()
var example1: MutableList<Item> = alternative_list_arrange(mutableListOf(from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)), mutableListOf(from_string("A"), from_string("B"), from_string("C")))
fun from_int(x: Int): Item {
    return (Int(value = x) as Item)
}

fun from_string(s: String): Item {
    return (Str(value = s) as Item)
}

fun item_to_string(it: Item): String {
    return (when (it) {
    is Int -> run {
    var v: Int = (((it.toInt())).value).toInt()
    v.toString()
}
    is Str -> run {
    var s: String = ((it as Str)).value
    s
}
} as String)
}

fun alternative_list_arrange(first: MutableList<Item>, second: MutableList<Item>): MutableList<Item> {
    var len1: Int = (first.size).toInt()
    var len2: Int = (second.size).toInt()
    var abs_len: Int = (if (len1 > len2) len1 else len2).toInt()
    var result: MutableList<Item> = mutableListOf<Item>()
    var i: Int = (0).toInt()
    while (i < abs_len) {
        if (i < len1) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(first[i]!!); _tmp }
        }
        if (i < len2) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(second[i]!!); _tmp }
        }
        i = i + 1
    }
    return result
}

fun list_to_string(xs: MutableList<Item>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < xs.size) {
        s = s + item_to_string(xs[i]!!)
        if (i < (xs.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(list_to_string(example1))
        var example2: MutableList<Item> = alternative_list_arrange(mutableListOf(from_string("A"), from_string("B"), from_string("C")), mutableListOf(from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)))
        println(list_to_string(example2))
        var example3: MutableList<Item> = alternative_list_arrange(mutableListOf(from_string("X"), from_string("Y"), from_string("Z")), mutableListOf(from_int(9), from_int(8), from_int(7), from_int(6)))
        println(list_to_string(example3))
        var example4: MutableList<Item> = alternative_list_arrange(mutableListOf(from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)), mutableListOf<Item>())
        println(list_to_string(example4))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
