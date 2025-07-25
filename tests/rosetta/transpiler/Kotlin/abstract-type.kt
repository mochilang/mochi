var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

sealed class Beast
data class Dog(var kind: String, var name: String) : Beast()
data class Cat(var kind: String, var name: String) : Beast()
fun beastKind(b: Beast): String {
    return when (b) {
    is Dog -> run {
    val k: String = (b as Dog).kind
    k
}
    is Cat -> run {
    val k: String = (b as Cat).kind
    k
}
} as String
}

fun beastName(b: Beast): String {
    return when (b) {
    is Dog -> run {
    val n: String = (b as Dog).name
    n
}
    is Cat -> run {
    val n: String = (b as Cat).name
    n
}
} as String
}

fun beastCry(b: Beast): String {
    return when (b) {
    is Dog -> run {
    "Woof"
}
    is Cat -> run {
    "Meow"
}
} as String
}

fun bprint(b: Beast): Unit {
    println(((((beastName(b) + ", who's a ") + beastKind(b)) + ", cries: \"") + beastCry(b)) + "\".")
}

fun user_main(): Unit {
    val d: Beast = Dog(kind = "labrador", name = "Max")
    val c: Beast = Cat(kind = "siamese", name = "Sammy")
    bprint(d)
    bprint(c)
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
