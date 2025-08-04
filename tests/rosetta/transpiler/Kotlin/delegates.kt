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

typealias Fn = () -> String
data class Delegator(var delegate: MutableMap<String, () -> String> = mutableMapOf<String, () -> String>())
var a: Delegator = Delegator(delegate = mutableMapOf<String, () -> String>())
fun operation(d: Delegator): String {
    if ("thing" in d.delegate) {
        return (((d.delegate)["thing"]!!)() as String)
    }
    return "default implementation"
}

fun newDelegate(): MutableMap<String, Fn> {
    var m: MutableMap<String, Fn> = mutableMapOf<String, Fn>()
    (m)["thing"] = {"delegate implementation" }
    return (m as MutableMap<String, Fn>)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(operation(a))
        a.delegate = mutableMapOf<String, () -> String>()
        println(operation(a))
        a.delegate = newDelegate()
        println(operation(a))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
