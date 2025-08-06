import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun encrypt(input_string: String, key: Int): String {
    if (key <= 0) {
        panic("Height of grid can't be 0 or negative")
    }
    if ((key == 1) || (input_string.length <= key)) {
        return input_string
    }
    var lowest: Int = key - 1
    var temp_grid: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var i: Int = 0
    while (i < key) {
        temp_grid = run { val _tmp = temp_grid.toMutableList(); _tmp.add(mutableListOf<String>()); _tmp }
        i = i + 1
    }
    var position: Int = 0
    while (position < input_string.length) {
        var num: BigInteger = ((Math.floorMod(position, (lowest * 2))).toBigInteger())
        var alt = ((lowest * 2)).toBigInteger().subtract((num).toBigInteger())
        if (num.compareTo((alt as Int).toBigInteger()) > 0) {
            num = (alt as Int).toBigInteger()
        }
        var row: MutableList<String> = temp_grid[(num).toInt()]!!
        row = run { val _tmp = row.toMutableList(); _tmp.add(input_string.substring(position, position + 1)); _tmp }
        _listSet(temp_grid, (num).toInt(), (row as MutableList<String>))
        position = position + 1
    }
    var output: String = ""
    i = 0
    while (i < key) {
        var row: MutableList<String> = temp_grid[i]!!
        var j: Int = 0
        while (j < row.size) {
            output = output + row[j]!!
            j = j + 1
        }
        i = i + 1
    }
    return output
}

fun decrypt(input_string: String, key: Int): String {
    if (key <= 0) {
        panic("Height of grid can't be 0 or negative")
    }
    if (key == 1) {
        return input_string
    }
    var lowest: Int = key - 1
    var counts: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < key) {
        counts = run { val _tmp = counts.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var pos: Int = 0
    while (pos < input_string.length) {
        var num: BigInteger = ((Math.floorMod(pos, (lowest * 2))).toBigInteger())
        var alt = ((lowest * 2)).toBigInteger().subtract((num).toBigInteger())
        if (num.compareTo((alt as Int).toBigInteger()) > 0) {
            num = (alt as Int).toBigInteger()
        }
        _listSet(counts, (num).toInt(), counts[(num).toInt()]!! + 1)
        pos = pos + 1
    }
    var grid: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var counter: Int = 0
    i = 0
    while (i < key) {
        var length: Int = counts[i]!!
        var slice: String = input_string.substring(counter, counter + length)
        var row: MutableList<String> = mutableListOf<String>()
        var j: Int = 0
        while (j < slice.length) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(slice[j].toString()); _tmp }
            j = j + 1
        }
        grid = run { val _tmp = grid.toMutableList(); _tmp.add(row); _tmp }
        counter = counter + length
        i = i + 1
    }
    var indices: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < key) {
        indices = run { val _tmp = indices.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var output: String = ""
    pos = 0
    while (pos < input_string.length) {
        var num: BigInteger = ((Math.floorMod(pos, (lowest * 2))).toBigInteger())
        var alt = ((lowest * 2)).toBigInteger().subtract((num).toBigInteger())
        if (num.compareTo((alt as Int).toBigInteger()) > 0) {
            num = (alt as Int).toBigInteger()
        }
        output = output + (((grid[(num).toInt()]!!) as MutableList<String>))[indices[(num).toInt()]!!]!!
        _listSet(indices, (num).toInt(), indices[(num).toInt()]!! + 1)
        pos = pos + 1
    }
    return output
}

fun bruteforce(input_string: String): MutableMap<Int, String> {
    var results: MutableMap<Int, String> = mutableMapOf<Int, String>()
    var key_guess: Int = 1
    while (key_guess < input_string.length) {
        (results)[key_guess] = decrypt(input_string, key_guess)
        key_guess = key_guess + 1
    }
    return results
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encrypt("Hello World", 4))
        println(decrypt("HWe olordll", 4))
        var bf: MutableMap<Int, String> = bruteforce("HWe olordll")
        println((bf)[4] as String)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
