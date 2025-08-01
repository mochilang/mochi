import java.math.BigInteger

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

var width: Int = 81
var height: Int = 5
var lines: MutableList<String> = mutableListOf<String>()
var stack: MutableList<MutableMap<String, Int>> = mutableListOf(mutableMapOf<String, Int>("start" to (0), "len" to (width), "index" to (1)))
fun setChar(s: String, idx: Int, ch: String): String {
    return (s.substring(0, idx) + ch) + s.substring(idx + 1, s.length)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (i in 0 until height) {
            var row: String = ""
            var j: Int = 0
            while (j < width) {
                row = row + "*"
                j = j + 1
            }
            lines = run { val _tmp = lines.toMutableList(); _tmp.add(row); _tmp } as MutableList<String>
        }
        while (stack.size > 0) {
            var frame: MutableMap<String, Int> = stack[stack.size - 1]!!
            stack = stack.subList(0, stack.size - 1)
            var start: Int = (frame)["start"] as Int
            var lenSeg: Int = (frame)["len"] as Int
            var index: Int = (frame)["index"] as Int
            var seg: Int = (lenSeg / 3).toInt()
            if (seg == 0) {
                continue
            }
            var i: Int = index
            while (i < height) {
                var j: BigInteger = (start + seg).toBigInteger()
                while (j.compareTo((start + (2 * seg)).toBigInteger()) < 0) {
                    (lines[i]) = setChar(lines[i]!!, j.toInt(), " ")
                    j = j.add(1.toBigInteger())
                }
                i = i + 1
            }
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(mutableMapOf<String, Int>("start" to (start), "len" to (seg), "index" to (index + 1))); _tmp } as MutableList<MutableMap<String, Int>>
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(mutableMapOf<String, Int>("start" to (start + (seg * 2)), "len" to (seg), "index" to (index + 1))); _tmp } as MutableList<MutableMap<String, Int>>
        }
        for (line in lines) {
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
