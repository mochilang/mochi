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

var width: Int = 320
var height: Int = 240
var img: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
var y: Int = 0
fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (y < height) {
            var row: MutableList<String> = mutableListOf<String>()
            var x: Int = 0
            while (x < width) {
                row = run { val _tmp = row.toMutableList(); _tmp.add("green"); _tmp }
                x = x + 1
            }
            img = run { val _tmp = img.toMutableList(); _tmp.add(row); _tmp }
            y = y + 1
        }
        (img[100]!!)[100] = "red"
        println(("The color of the pixel at (  0,   0) is " + (((img[0]!!) as MutableList<String>))[0]!!) + ".")
        println(("The color of the pixel at (100, 100) is " + (((img[100]!!) as MutableList<String>))[100]!!) + ".")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
