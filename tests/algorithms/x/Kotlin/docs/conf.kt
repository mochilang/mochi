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

var pyproject: String = "[project]\nname = \"thealgorithms-python\""
var project: String = parse_project_name(pyproject)
fun parse_project_name(toml: String): String {
    var i: Int = (0).toInt()
    var name: String = ""
    var n: Int = (toml.length).toInt()
    while ((i + 4) < n) {
        if ((((((toml[i].toString() == "n") && (toml[i + 1].toString() == "a") as Boolean)) && (toml[i + 2].toString() == "m") as Boolean)) && (toml[i + 3].toString() == "e")) {
            i = i + 4
            while ((i < n) && (toml[i].toString() != "\"")) {
                i = i + 1
            }
            i = i + 1
            while ((i < n) && (toml[i].toString() != "\"")) {
                name = name + toml[i].toString()
                i = i + 1
            }
            return name
        }
        i = i + 1
    }
    return name
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(project)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
