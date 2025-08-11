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

var logins1: MutableList<String> = mutableListOf("135", "259", "235", "189", "690", "168", "120", "136", "289", "589", "160", "165", "580", "369", "250", "280")
fun parse_int(s: String): Int {
    var value: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        value = (value * 10) + ((c.toBigInteger().toInt()))
        i = i + 1
    }
    return value
}

fun join(xs: MutableList<String>): String {
    var s: String = ""
    var i: Int = (0).toInt()
    while (i < xs.size) {
        s = s + xs[i]!!
        i = i + 1
    }
    return s
}

fun contains(xs: MutableList<String>, c: String): Boolean {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == c) {
            return true
        }
        i = i + 1
    }
    return false
}

fun index_of(xs: MutableList<String>, c: String): Int {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == c) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun remove_at(xs: MutableList<String>, idx: Int): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (i != idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun unique_chars(logins: MutableList<String>): MutableList<String> {
    var chars: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < logins.size) {
        var login: String = logins[i]!!
        var j: Int = (0).toInt()
        while (j < login.length) {
            var c: String = login[j].toString()
            if (!((chars.contains(c)) as Boolean)) {
                chars = run { val _tmp = chars.toMutableList(); _tmp.add(c); _tmp }
            }
            j = j + 1
        }
        i = i + 1
    }
    return chars
}

fun satisfies(permutation: MutableList<String>, logins: MutableList<String>): Boolean {
    var i: Int = (0).toInt()
    while (i < logins.size) {
        var login: String = logins[i]!!
        var i0: Int = (index_of(permutation, login[0].toString())).toInt()
        var i1: Int = (index_of(permutation, login[1].toString())).toInt()
        var i2: Int = (index_of(permutation, login[2].toString())).toInt()
        if (!(((i0 < i1) && (i1 < i2)) as Boolean)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun search(chars: MutableList<String>, current: MutableList<String>, logins: MutableList<String>): String {
    if (chars.size == 0) {
        if (((satisfies(current, logins)) as Boolean)) {
            return join(current)
        }
        return ""
    }
    var i: Int = (0).toInt()
    while (i < chars.size) {
        var c: String = chars[i]!!
        var rest: MutableList<String> = remove_at(chars, i)
        var next = run { val _tmp = current.toMutableList(); _tmp.add(c); _tmp }
        var res: String = search(rest, (next as MutableList<String>), logins)
        if (res != "") {
            return res
        }
        i = i + 1
    }
    return ""
}

fun find_secret_passcode(logins: MutableList<String>): Int {
    var chars: MutableList<String> = unique_chars(logins)
    var s: String = search(chars, mutableListOf<String>(), logins)
    if (s == "") {
        return 0 - 1
    }
    return parse_int(s)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(find_secret_passcode(logins1).toString())
        var logins2: MutableList<String> = mutableListOf("426", "281", "061", "819", "268", "406", "420", "428", "209", "689", "019", "421", "469", "261", "681", "201")
        println(find_secret_passcode(logins2).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
