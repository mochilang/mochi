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

fun merge(base: MutableMap<String, Any?>, update: MutableMap<String, Any?>): MutableMap<String, Any?> {
    var result: MutableMap<String, Any?> = mutableMapOf<Any?, Any?>() as MutableMap<String, Any?>
    for (k in base.keys) {
        (result)[k] = (base)[k] as Any?
    }
    for (k in update.keys) {
        (result)[k] = (update)[k] as Any?
    }
    return result
}

fun user_main(): Unit {
    var base: MutableMap<String, Any?> = mutableMapOf<String, Any?>("name" to ("Rocket Skates"), "price" to (12.75), "color" to ("yellow")) as MutableMap<String, Any?>
    var update: MutableMap<String, Any?> = mutableMapOf<String, Any?>("price" to (15.25), "color" to ("red"), "year" to (1974)) as MutableMap<String, Any?>
    var result: MutableMap<String, Any?> = merge(base, update)
    println(result)
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
