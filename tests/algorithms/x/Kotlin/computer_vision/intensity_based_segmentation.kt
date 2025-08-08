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

fun segment_image(image: MutableList<MutableList<Int>>, thresholds: MutableList<Int>): MutableList<MutableList<Int>> {
    var segmented: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < image.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < (image[i]!!).size) {
            var pixel: Int = (((image[i]!!) as MutableList<Int>))[j]!!
            var label: Int = 0
            var k: Int = 0
            while (k < thresholds.size) {
                if (pixel > thresholds[k]!!) {
                    label = k + 1
                }
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(label); _tmp }
            j = j + 1
        }
        segmented = run { val _tmp = segmented.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return segmented
}

fun user_main(): Unit {
    var image: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(80, 120, 180), mutableListOf(40, 90, 150), mutableListOf(20, 60, 100))
    var thresholds: MutableList<Int> = mutableListOf(50, 100, 150)
    var segmented: MutableList<MutableList<Int>> = segment_image(image, thresholds)
    println(segmented)
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
