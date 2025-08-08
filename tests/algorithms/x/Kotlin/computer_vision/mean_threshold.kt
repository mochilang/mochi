import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var img: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(10, 200, 50), mutableListOf(100, 150, 30), mutableListOf(90, 80, 220))
var result: MutableList<MutableList<Int>> = mean_threshold(img)
fun mean_threshold(image: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var height: Int = image.size
    var width: Int = (image[0]!!).size
    var total: Int = 0
    var i: Int = 0
    while (i < height) {
        var j: Int = 0
        while (j < width) {
            total = total + (((image[i]!!) as MutableList<Int>))[j]!!
            j = j + 1
        }
        i = i + 1
    }
    var mean: Int = total / (height * width)
    i = 0
    while (i < height) {
        var j: Int = 0
        while (j < width) {
            if ((((image[i]!!) as MutableList<Int>))[j]!! > mean) {
                _listSet(image[i]!!, j, 255)
            } else {
                _listSet(image[i]!!, j, 0)
            }
            j = j + 1
        }
        i = i + 1
    }
    return image
}

fun print_image(image: MutableList<MutableList<Int>>): Unit {
    var i: Int = 0
    while (i < image.size) {
        println(image[i]!!)
        i = i + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        print_image(result)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
