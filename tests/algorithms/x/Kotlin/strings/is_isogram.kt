fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun index_of(s: String, ch: String): Int {
    var i: Int = (0).toInt()
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var idx: Int = (index_of(upper, ch)).toInt()
    if (idx >= 0) {
        return 65 + idx
    }
    idx = index_of(lower, ch)
    if (idx >= 0) {
        return 97 + idx
    }
    return 0 - 1
}

fun chr(n: Int): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    if ((n >= 65) && (n < 91)) {
        return _sliceStr(upper, n - 65, n - 64)
    }
    if ((n >= 97) && (n < 123)) {
        return _sliceStr(lower, n - 97, n - 96)
    }
    return "?"
}

fun to_lower_char(c: String): String {
    var code: Int = (ord(c)).toInt()
    if ((code >= 65) && (code <= 90)) {
        return chr(code + 32)
    }
    return c
}

fun is_alpha(c: String): Boolean {
    var code: Int = (ord(c)).toInt()
    return (((((code >= 65) && (code <= 90) as Boolean)) || (((code >= 97) && (code <= 122) as Boolean))) as Boolean)
}

fun is_isogram(s: String): Boolean {
    var seen: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s[i].toString()
        if (!is_alpha(ch)) {
            panic("String must only contain alphabetic characters.")
        }
        var lower: String = to_lower_char(ch)
        if (index_of(seen, lower) >= 0) {
            return false
        }
        seen = seen + lower
        i = i + 1
    }
    return true
}

fun main() {
    println(is_isogram("Uncopyrightable").toString())
    println(is_isogram("allowance").toString())
}
