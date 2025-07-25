fun input(): String = readLine() ?: ""

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

data class SomeStruct(var runtimeFields: MutableMap<String, String>)
fun user_main(): Unit {
    var ss: SomeStruct = SomeStruct(runtimeFields = mutableMapOf<Any?, Any?>() as MutableMap<String, String>)
    println("Create two fields at runtime: \n")
    var i: Int = 1
    while (i <= 2) {
        println(("  Field #" + i.toString()) + ":\n")
        println("       Enter name  : ")
        val name: String = input()
        println("       Enter value : ")
        val value: String = input()
        var fields: MutableMap<String, String> = ss.runtimeFields
        (fields)[name] = value
        ss.runtimeFields = fields
        println("\n")
        i = i + 1
    }
    while (true) {
        println("Which field do you want to inspect ? ")
        val name: String = input()
        if (name in ss.runtimeFields) {
            val value: String = (ss.runtimeFields)[name] as String
            println(("Its value is '" + value) + "'")
            return
        } else {
            println("There is no field of that name, try again\n")
        }
    }
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
