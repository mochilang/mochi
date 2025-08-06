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

var SQUARE: MutableList<MutableList<String>> = mutableListOf(mutableListOf("a", "b", "c", "d", "e"), mutableListOf("f", "g", "h", "i", "k"), mutableListOf("l", "m", "n", "o", "p"), mutableListOf("q", "r", "s", "t", "u"), mutableListOf("v", "w", "x", "y", "z"))
fun index_of(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun to_lower_without_spaces(message: String, replace_j: Boolean): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var res: String = ""
    var i: Int = 0
    while (i < message.length) {
        var ch: String = message[i].toString()
        var pos: Int = index_of(upper, ch)
        if (pos >= 0) {
            ch = lower[pos].toString()
        }
        if (ch != " ") {
            if (replace_j && (ch == "j")) {
                ch = "i"
            }
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun letter_to_numbers(letter: String): MutableList<Int> {
    var r: Int = 0
    while (r < SQUARE.size) {
        var c: Int = 0
        while (c < (SQUARE[r]!!).size) {
            if ((((SQUARE[r]!!) as MutableList<String>))[c]!! == letter) {
                return mutableListOf<Int>(r + 1, c + 1)
            }
            c = c + 1
        }
        r = r + 1
    }
    return mutableListOf<Int>(0, 0)
}

fun numbers_to_letter(row: Int, col: Int): String {
    return (((SQUARE[row - 1]!!) as MutableList<String>))[col - 1]!!
}

fun encode(message: String): String {
    var clean: String = to_lower_without_spaces(message, true)
    var l: Int = clean.length
    var rows: MutableList<Int> = mutableListOf<Int>()
    var cols: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < l) {
        var nums: MutableList<Int> = letter_to_numbers(clean[i].toString())
        rows = run { val _tmp = rows.toMutableList(); _tmp.add(nums[0]!!); _tmp }
        cols = run { val _tmp = cols.toMutableList(); _tmp.add(nums[1]!!); _tmp }
        i = i + 1
    }
    var seq: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < l) {
        seq = run { val _tmp = seq.toMutableList(); _tmp.add(rows[i]!!); _tmp }
        i = i + 1
    }
    i = 0
    while (i < l) {
        seq = run { val _tmp = seq.toMutableList(); _tmp.add(cols[i]!!); _tmp }
        i = i + 1
    }
    var encoded: String = ""
    i = 0
    while (i < l) {
        var r: Int = seq[2 * i]!!
        var c: Int = seq[(2 * i) + 1]!!
        encoded = encoded + numbers_to_letter(r, c)
        i = i + 1
    }
    return encoded
}

fun decode(message: String): String {
    var clean: String = to_lower_without_spaces(message, false)
    var l: Int = clean.length
    var first: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < l) {
        var nums: MutableList<Int> = letter_to_numbers(clean[i].toString())
        first = run { val _tmp = first.toMutableList(); _tmp.add(nums[0]!!); _tmp }
        first = run { val _tmp = first.toMutableList(); _tmp.add(nums[1]!!); _tmp }
        i = i + 1
    }
    var top: MutableList<Int> = mutableListOf<Int>()
    var bottom: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < l) {
        top = run { val _tmp = top.toMutableList(); _tmp.add(first[i]!!); _tmp }
        bottom = run { val _tmp = bottom.toMutableList(); _tmp.add(first[i + l]!!); _tmp }
        i = i + 1
    }
    var decoded: String = ""
    i = 0
    while (i < l) {
        var r: Int = top[i]!!
        var c: Int = bottom[i]!!
        decoded = decoded + numbers_to_letter(r, c)
        i = i + 1
    }
    return decoded
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encode("testmessage"))
        println(encode("Test Message"))
        println(encode("test j"))
        println(encode("test i"))
        println(decode("qtltbdxrxlk"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
