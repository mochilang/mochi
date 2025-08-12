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

var seed: Int = (1).toInt()
var integers: MutableList<Int> = mutableListOf(0, 1, 2, 3, 4, 5, 6, 7)
var strings: MutableList<String> = mutableListOf("python", "says", "hello", "!")
fun rand(): Int {
    seed = (((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())).toInt()
    return seed / 65536
}

fun randint(a: Int, b: Int): Int {
    var r: Int = (rand()).toInt()
    return a + (Math.floorMod(r, ((b - a) + 1)))
}

fun fisher_yates_shuffle_int(data: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = data
    var i: Int = (0).toInt()
    while (i < res.size) {
        var a: Int = (randint(0, res.size - 1)).toInt()
        var b: Int = (randint(0, res.size - 1)).toInt()
        var temp: Int = (res[a]!!).toInt()
        _listSet(res, a, res[b]!!)
        _listSet(res, b, temp)
        i = i + 1
    }
    return res
}

fun fisher_yates_shuffle_str(data: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = data
    var i: Int = (0).toInt()
    while (i < res.size) {
        var a: Int = (randint(0, res.size - 1)).toInt()
        var b: Int = (randint(0, res.size - 1)).toInt()
        var temp: String = res[a]!!
        _listSet(res, a, res[b]!!)
        _listSet(res, b, temp)
        i = i + 1
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Fisher-Yates Shuffle:")
        println((("List " + integers.toString()) + " ") + strings.toString())
        println((("FY Shuffle " + fisher_yates_shuffle_int(integers).toString()) + " ") + fisher_yates_shuffle_str(strings).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
