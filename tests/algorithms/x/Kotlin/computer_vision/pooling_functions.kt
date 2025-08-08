fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun maxpooling(arr: MutableList<MutableList<Int>>, size: Int, stride: Int): MutableList<MutableList<Int>> {
    var n: Int = arr.size
    if ((n == 0) || ((arr[0]!!).size != n)) {
        panic("The input array is not a square matrix")
    }
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while ((i + size) <= n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while ((j + size) <= n) {
            var max_val: Int = (((arr[i]!!) as MutableList<Int>))[j]!!
            var r: Int = i
            while (r < (i + size)) {
                var c: Int = j
                while (c < (j + size)) {
                    var _val: Int = (((arr[r]!!) as MutableList<Int>))[c]!!
                    if (_val > max_val) {
                        max_val = _val
                    }
                    c = c + 1
                }
                r = r + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(max_val); _tmp }
            j = j + stride
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + stride
    }
    return result
}

fun avgpooling(arr: MutableList<MutableList<Int>>, size: Int, stride: Int): MutableList<MutableList<Int>> {
    var n: Int = arr.size
    if ((n == 0) || ((arr[0]!!).size != n)) {
        panic("The input array is not a square matrix")
    }
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while ((i + size) <= n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while ((j + size) <= n) {
            var sum: Int = 0
            var r: Int = i
            while (r < (i + size)) {
                var c: Int = j
                while (c < (j + size)) {
                    sum = sum + (((arr[r]!!) as MutableList<Int>))[c]!!
                    c = c + 1
                }
                r = r + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum / (size * size)); _tmp }
            j = j + stride
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + stride
    }
    return result
}

fun print_matrix(mat: MutableList<MutableList<Int>>): Unit {
    var i: Int = 0
    while (i < mat.size) {
        var line: String = ""
        var j: Int = 0
        while (j < (mat[i]!!).size) {
            line = line + ((((mat[i]!!) as MutableList<Int>))[j]!!).toString()
            if (j < ((mat[i]!!).size - 1)) {
                line = line + " "
            }
            j = j + 1
        }
        println(line)
        i = i + 1
    }
}

fun user_main(): Unit {
    var arr1: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2, 3, 4), mutableListOf(5, 6, 7, 8), mutableListOf(9, 10, 11, 12), mutableListOf(13, 14, 15, 16))
    var arr2: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(147, 180, 122), mutableListOf(241, 76, 32), mutableListOf(126, 13, 157))
    print_matrix(maxpooling(arr1, 2, 2))
    print_matrix(maxpooling(arr2, 2, 1))
    print_matrix(avgpooling(arr1, 2, 2))
    print_matrix(avgpooling(arr2, 2, 1))
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
