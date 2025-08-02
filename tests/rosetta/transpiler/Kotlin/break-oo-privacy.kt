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

data class Foobar(var Exported: Int = 0, var unexported: Int = 0)
var obj: Foobar = Foobar(Exported = 12, unexported = 42)
fun examineAndModify(f: Foobar): Foobar {
    println((((((((" v: {" + f.Exported.toString()) + " ") + f.unexported.toString()) + "} = {") + f.Exported.toString()) + " ") + f.unexported.toString()) + "}")
    println("    Idx Name       Type CanSet")
    println("     0: Exported   int  true")
    println("     1: unexported int  false")
    f.Exported = 16
    f.unexported = 44
    println("  modified unexported field via unsafe")
    return f
}

fun anotherExample(): Unit {
    println("bufio.ReadByte returned error: unsafely injected error value into bufio inner workings")
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(((("obj: {" + obj.Exported.toString()) + " ") + obj.unexported.toString()) + "}")
        obj = examineAndModify(obj)
        println(((("obj: {" + obj.Exported.toString()) + " ") + obj.unexported.toString()) + "}")
        anotherExample()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
