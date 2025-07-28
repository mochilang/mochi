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

fun Node(value: String, next: Any?, prev: Any?): MutableMap<String, Any?> {
    return mutableMapOf<String, Any?>("value" to (value), "next" to (next), "prev" to (prev))
}

fun user_main(): Unit {
    var a: MutableMap<String, Any?> = Node("A", null as Any?, null as Any?)
    var b: MutableMap<String, Any?> = Node("B", null as Any?, a as Any?)
    (a)["next"] as Any? = b as Any?
    var c: MutableMap<String, Any?> = Node("C", null as Any?, b as Any?)
    (b)["next"] as Any? = c as Any?
    var p: MutableMap<String, Any?> = a
    var line: String = ""
    while (p != null) {
        line = line + (p)["value"] as String
        p = ((p)["next"] as Any?) as MutableMap<String, Any?>
        if (p != null) {
            line = line + " "
        }
    }
    println(line)
    p = c
    line = ""
    while (p != null) {
        line = line + (p)["value"] as String
        p = ((p)["prev"] as Any?) as MutableMap<String, Any?>
        if (p != null) {
            line = line + " "
        }
    }
    println(line)
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
