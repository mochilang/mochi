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

var LOWER: String = "abcdefghijklmnopqrstuvwxyz"
var UPPER: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var PUNCT: String = "!\"#$%&'()*+,-./:;<=>?@[\\]^_{|}~"
fun to_lowercase(s: String): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        var j: Int = (0).toInt()
        var found: Boolean = false
        while (j < UPPER.length) {
            if (c == UPPER[j].toString()) {
                res = res + LOWER[j].toString()
                found = true
                break
            }
            j = j + 1
        }
        if (!found) {
            res = res + c
        }
        i = i + 1
    }
    return res
}

fun is_punct(c: String): Boolean {
    var i: Int = (0).toInt()
    while (i < PUNCT.length) {
        if (c == PUNCT[i].toString()) {
            return true
        }
        i = i + 1
    }
    return false
}

fun clean_text(text: String, keep_newlines: Boolean): String {
    var lower: String = to_lowercase(text)
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < lower.length) {
        var ch: String = lower[i].toString()
        if (((is_punct(ch)) as Boolean)) {
        } else {
            if (ch == "\n") {
                if ((keep_newlines as Boolean)) {
                    res = res + "\n"
                }
            } else {
                res = res + ch
            }
        }
        i = i + 1
    }
    return res
}

fun split(s: String, sep: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s[i].toString()
        if (ch == sep) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
            current = ""
        } else {
            current = current + ch
        }
        i = i + 1
    }
    res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
    return res
}

fun contains(s: String, sub: String): Boolean {
    var n: Int = (s.length).toInt()
    var m: Int = (sub.length).toInt()
    if (m == 0) {
        return true
    }
    var i: Int = (0).toInt()
    while (i <= (n - m)) {
        var j: Int = (0).toInt()
        var is_match: Boolean = true
        while (j < m) {
            if (s[i + j].toString() != sub[j].toString()) {
                is_match = false
                break
            }
            j = j + 1
        }
        if ((is_match as Boolean)) {
            return true
        }
        i = i + 1
    }
    return false
}

fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun round3(x: Double): Double {
    return floor((x * 1000.0) + 0.5) / 1000.0
}

fun ln(x: Double): Double {
    var t: Double = (x - 1.0) / (x + 1.0)
    var term: Double = t
    var sum: Double = 0.0
    var k: Int = (1).toInt()
    while (k <= 99) {
        sum = sum + (term / ((k.toDouble())))
        term = (term * t) * t
        k = k + 2
    }
    return 2.0 * sum
}

fun log10(x: Double): Double {
    return ln(x) / ln(10.0)
}

fun term_frequency(term: String, document: String): Int {
    var clean: String = clean_text(document, false)
    var tokens: MutableList<String> = split(clean, " ")
    var t: String = to_lowercase(term)
    var count: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < tokens.size) {
        if ((tokens[i]!! != "") && (tokens[i]!! == t)) {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

fun document_frequency(term: String, corpus: String): MutableList<Int> {
    var clean: String = clean_text(corpus, true)
    var docs: MutableList<String> = split(clean, "\n")
    var t: String = to_lowercase(term)
    var matches: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < docs.size) {
        if (docs[i]!!.contains(t)) {
            matches = matches + 1
        }
        i = i + 1
    }
    return mutableListOf(matches, docs.size)
}

fun inverse_document_frequency(df: Int, n: Int, smoothing: Boolean): Double {
    if ((smoothing as Boolean)) {
        if (n == 0) {
            panic("log10(0) is undefined.")
        }
        var ratio: Double = ((n.toDouble())) / (1.0 + ((df.toDouble())))
        var l: Double = log10(ratio)
        var result: Double = round3(1.0 + l)
        println(result)
        return result
    }
    if (df == 0) {
        panic("df must be > 0")
    }
    if (n == 0) {
        panic("log10(0) is undefined.")
    }
    var ratio: Double = ((n.toDouble())) / ((df.toDouble()))
    var l: Double = log10(ratio)
    var result: Double = round3(l)
    println(result)
    return result
}

fun tf_idf(tf: Int, idf: Double): Double {
    var prod: Double = ((tf.toDouble())) * idf
    var result: Double = round3(prod)
    println(result)
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(term_frequency("to", "To be, or not to be"))
        var corpus: String = "This is the first document in the corpus.\nThIs is the second document in the corpus.\nTHIS is the third document in the corpus."
        println(document_frequency("first", corpus).toString())
        var idf_val: Double = inverse_document_frequency(1, 3, false)
        tf_idf(2, idf_val)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
