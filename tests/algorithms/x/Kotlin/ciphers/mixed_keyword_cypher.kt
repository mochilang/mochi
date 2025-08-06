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

var UPPER: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var LOWER: String = "abcdefghijklmnopqrstuvwxyz"
fun to_upper(s: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s[i].toString()
        var j: Int = 0
        var found: Boolean = false
        while (j < 26) {
            if (ch == LOWER[j].toString()) {
                res = res + UPPER[j].toString()
                found = true
                break
            }
            j = j + 1
        }
        if (found == false) {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun contains(xs: MutableList<String>, x: String): Boolean {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return true
        }
        i = i + 1
    }
    return false
}

fun contains_char(s: String, ch: String): Boolean {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return true
        }
        i = i + 1
    }
    return false
}

fun get_value(keys: MutableList<String>, values: MutableList<String>, key: String): String {
    var i: Int = 0
    while (i < keys.size) {
        if (keys[i]!! == key) {
            return values[i]!!
        }
        i = i + 1
    }
    return (null as String)
}

fun print_mapping(keys: MutableList<String>, values: MutableList<String>): Unit {
    var s: String = "{"
    var i: Int = 0
    while (i < keys.size) {
        s = ((((s + "'") + keys[i]!!) + "': '") + values[i]!!) + "'"
        if ((i + 1) < keys.size) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "}"
    println(s)
}

fun mixed_keyword(keyword: String, plaintext: String, verbose: Boolean): String {
    var alphabet: String = UPPER
    var keyword_u: String = to_upper(keyword)
    var plaintext_u: String = to_upper(plaintext)
    var unique: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < keyword_u.length) {
        var ch: String = keyword_u[i].toString()
        if (contains_char(alphabet, ch) && (unique.contains(ch) == false)) {
            unique = run { val _tmp = unique.toMutableList(); _tmp.add(ch); _tmp }
        }
        i = i + 1
    }
    var num_unique: Int = unique.size
    var shifted: MutableList<String> = mutableListOf<String>()
    i = 0
    while (i < unique.size) {
        shifted = run { val _tmp = shifted.toMutableList(); _tmp.add(unique[i]!!); _tmp }
        i = i + 1
    }
    i = 0
    while (i < alphabet.length) {
        var ch: String = alphabet[i].toString()
        if (unique.contains(ch) == false) {
            shifted = run { val _tmp = shifted.toMutableList(); _tmp.add(ch); _tmp }
        }
        i = i + 1
    }
    var modified: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var k: Int = 0
    while (k < shifted.size) {
        var row: MutableList<String> = mutableListOf<String>()
        var r: Int = 0
        while ((r < num_unique) && ((k + r) < shifted.size)) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(shifted[k + r]!!); _tmp }
            r = r + 1
        }
        modified = run { val _tmp = modified.toMutableList(); _tmp.add(row); _tmp }
        k = k + num_unique
    }
    var keys: MutableList<String> = mutableListOf<String>()
    var values: MutableList<String> = mutableListOf<String>()
    var column: Int = 0
    var letter_index: Int = 0
    while (column < num_unique) {
        var row_idx: Int = 0
        while (row_idx < modified.size) {
            var row: MutableList<String> = modified[row_idx]!!
            if (row.size <= column) {
                break
            }
            keys = run { val _tmp = keys.toMutableList(); _tmp.add(alphabet[letter_index].toString()); _tmp }
            values = run { val _tmp = values.toMutableList(); _tmp.add(row[column]!!); _tmp }
            letter_index = letter_index + 1
            row_idx = row_idx + 1
        }
        column = column + 1
    }
    if ((verbose as Boolean)) {
        print_mapping(keys, values)
    }
    var result: String = ""
    i = 0
    while (i < plaintext_u.length) {
        var ch: String = plaintext_u[i].toString()
        var mapped: String = get_value(keys, values, ch)
        if (mapped == null) {
            result = result + ch
        } else {
            result = result + mapped
        }
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(mixed_keyword("college", "UNIVERSITY", true))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
