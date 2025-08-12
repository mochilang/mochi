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

var OPEN_TO_CLOSED: MutableMap<String, String> = mutableMapOf<String, String>("(" to (")"), "[" to ("]"), "{" to ("}"))
fun slice_without_last(xs: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun is_balanced(s: String): Boolean {
    var stack: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var symbol: String = s.substring(i, i + 1)
        if (symbol in OPEN_TO_CLOSED) {
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(symbol); _tmp }
        } else {
            if ((((symbol == ")") || (symbol == "]") as Boolean)) || (symbol == "}")) {
                if (stack.size == 0) {
                    return false
                }
                var top: String = stack[stack.size - 1]!!
                if ((OPEN_TO_CLOSED)[top] as String != symbol) {
                    return false
                }
                stack = slice_without_last(stack)
            }
        }
        i = i + 1
    }
    return stack.size == 0
}

fun user_main(): Unit {
    println(is_balanced(""))
    println(is_balanced("()"))
    println(is_balanced("[]"))
    println(is_balanced("{}"))
    println(is_balanced("()[]{}"))
    println(is_balanced("(())"))
    println(is_balanced("[["))
    println(is_balanced("([{}])"))
    println(is_balanced("(()[)]"))
    println(is_balanced("([)]"))
    println(is_balanced("[[()]]"))
    println(is_balanced("(()(()))"))
    println(is_balanced("]"))
    println(is_balanced("Life is a bowl of cherries."))
    println(is_balanced("Life is a bowl of che{}ies."))
    println(is_balanced("Life is a bowl of che}{ies."))
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
