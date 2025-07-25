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
        _nowSeed.toInt()
    } else {
        System.nanoTime().toInt()
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

var doors: MutableList<Any?> = mutableListOf()
fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (i in 0 until 100) {
            doors = run { val _tmp = doors.toMutableList(); _tmp.add(false); _tmp } as MutableList<Any?>
        }
        for (pass in 1 until 101) {
            var idx: Int = pass - 1
            while (idx < 100) {
                doors[idx] = !(doors[idx] as Boolean)
                idx = idx + pass
            }
        }
        for (row in 0 until 10) {
            var line: String = ""
            for (col in 0 until 10) {
                val idx: Int = (row * 10) + col
                if ((doors[idx]) as Boolean) {
                    line = line + "1"
                } else {
                    line = line + "0"
                }
                if (col < 9) {
                    line = line + " "
                }
            }
            println(line)
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
