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

fun get_value(keys: MutableList<String>, values: MutableList<String>, key: String): String {
    var i: Int = 0
    while (i < keys.size) {
        if (keys[i]!! == key) {
            return values[i]!!
        }
        i = i + 1
    }
    return (null as String)
}

fun contains_value(values: MutableList<String>, value: String): Boolean {
    var i: Int = 0
    while (i < values.size) {
        if (values[i]!! == value) {
            return true
        }
        i = i + 1
    }
    return false
}

fun backtrack(pattern: String, input_string: String, pi: Int, si: Int, keys: MutableList<String>, values: MutableList<String>): Boolean {
    if ((pi == pattern.length) && (si == input_string.length)) {
        return true
    }
    if ((pi == pattern.length) || (si == input_string.length)) {
        return false
    }
    var ch: String = pattern.substring(pi, pi + 1)
    var mapped: String = get_value(keys, values, ch)
    if (mapped != null) {
        if (input_string.substring(si, si + mapped.length) == mapped) {
            return backtrack(pattern, input_string, pi + 1, si + mapped.length, keys, values)
        }
        return false
    }
    var end: BigInteger = ((si + 1).toBigInteger())
    while (end.compareTo((input_string.length).toBigInteger()) <= 0) {
        var substr: String = input_string.substring(si, (end).toInt())
        if (((contains_value(values, substr)) as Boolean)) {
            end = end.add((1).toBigInteger())
            continue
        }
        var new_keys: MutableList<String> = run { val _tmp = keys.toMutableList(); _tmp.add(ch); _tmp }
        var new_values: MutableList<String> = run { val _tmp = values.toMutableList(); _tmp.add(substr); _tmp }
        if (((backtrack(pattern, input_string, pi + 1, (end.toInt()), (new_keys as MutableList<String>), (new_values as MutableList<String>))) as Boolean)) {
            return true
        }
        end = end.add((1).toBigInteger())
    }
    return false
}

fun match_word_pattern(pattern: String, input_string: String): Boolean {
    var keys: MutableList<String> = mutableListOf<String>()
    var values: MutableList<String> = mutableListOf<String>()
    return backtrack(pattern, input_string, 0, 0, keys, values)
}

fun user_main(): Unit {
    println(match_word_pattern("aba", "GraphTreesGraph"))
    println(match_word_pattern("xyx", "PythonRubyPython"))
    println(match_word_pattern("GG", "PythonJavaPython"))
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
