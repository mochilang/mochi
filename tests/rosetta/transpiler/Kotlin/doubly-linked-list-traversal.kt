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

var nodes: MutableMap<Int, MutableMap<String, Any?>> = mutableMapOf<Int, MutableMap<String, Any?>>()
var head: Int = 0 - 1
var tail: Int = 0 - 1
fun listString(): String {
    if (head == (0 - 1)) {
        return "<nil>"
    }
    var r: String = "[" + (((((nodes)[head] as MutableMap<String, Any?>) as MutableMap<String, Any?>))["value"]!!).toString()
    var id: Int? = ((((((nodes)[head] as MutableMap<String, Any?>) as MutableMap<String, Any?>))["next"] as Int).toInt())
    while (id != (0 - 1)) {
        r = (r + " ") + (((((nodes)[id] as MutableMap<String, Any?>) as MutableMap<String, Any?>))["value"]!!).toString()
        id = ((((nodes)[id] as MutableMap<String, Any?>) as MutableMap<String, Any?>))["next"] as Int
    }
    r = r + "]"
    return r
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(listString())
        (nodes)[0] = mutableMapOf<String, Any?>("value" to ("A"), "next" to (0 - 1), "prev" to (0 - 1))
        head = 0
        tail = 0
        (nodes)[1] = mutableMapOf<String, Any?>("value" to ("B"), "next" to (0 - 1), "prev" to (0))
        ((nodes)[0] as MutableMap<String, Any?>)["next"] = (1 as Any?)
        tail = 1
        println(listString())
        (nodes)[2] = mutableMapOf<String, Any?>("value" to ("C"), "next" to (1), "prev" to (0))
        ((nodes)[1] as MutableMap<String, Any?>)["prev"] = (2 as Any?)
        ((nodes)[0] as MutableMap<String, Any?>)["next"] = (2 as Any?)
        println(listString())
        var out: String = "From tail:"
        var id: Int = tail
        while (id != (0 - 1)) {
            out = (out + " ") + (((((nodes)[id] as MutableMap<String, Any?>) as MutableMap<String, Any?>))["value"]!!).toString()
            id = ((((nodes)[id] as MutableMap<String, Any?>) as MutableMap<String, Any?>))["prev"] as Int
        }
        println(out)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
