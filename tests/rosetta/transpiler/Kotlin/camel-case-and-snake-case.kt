import java.math.BigInteger

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

fun trimSpace(s: String): String {
    var start: Int = 0
    while ((start < s.length) && (s.substring(start, start + 1) == " ")) {
        start = start + 1
    }
    var end: Int = s.length
    while ((end > start) && (s.substring(end - 1, end) == " ")) {
        end = end - 1
    }
    return s.substring(start, end)
}

fun isUpper(ch: String): Boolean {
    return ((ch >= "A") && (ch <= "Z")) as Boolean
}

fun padLeft(s: String, w: Int): String {
    var res: String = ""
    var n: BigInteger = (w - s.length).toBigInteger()
    while (n.compareTo(0.toBigInteger()) > 0) {
        res = res + " "
        n = n.subtract(1.toBigInteger())
    }
    return res + s
}

fun snakeToCamel(s: String): String {
    var s: String = s
    s = trimSpace(s)
    var out: String = ""
    var up: Boolean = false
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if ((((((ch == "_") || (ch == "-") as Boolean)) || (ch == " ") as Boolean)) || (ch == ".")) {
            up = true
            i = i + 1
            continue
        }
        if (i == 0) {
            out = out + (ch.toLowerCase()).toString()
            up = false
            i = i + 1
            continue
        }
        if (up as Boolean) {
            out = out + (ch.toUpperCase()).toString()
            up = false
        } else {
            out = out + ch
        }
        i = i + 1
    }
    return out
}

fun camelToSnake(s: String): String {
    var s: String = s
    s = trimSpace(s)
    var out: String = ""
    var prevUnd: Boolean = false
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if ((((ch == " ") || (ch == "-") as Boolean)) || (ch == ".")) {
            if ((!prevUnd as Boolean) && (out.length > 0)) {
                out = out + "_"
                prevUnd = true
            }
            i = i + 1
            continue
        }
        if (ch == "_") {
            if ((!prevUnd as Boolean) && (out.length > 0)) {
                out = out + "_"
                prevUnd = true
            }
            i = i + 1
            continue
        }
        if ((isUpper(ch)) as Boolean) {
            if ((i > 0) && (!prevUnd as Boolean)) {
                out = out + "_"
            }
            out = out + (ch.toLowerCase()).toString()
            prevUnd = false
        } else {
            out = out + (ch.toLowerCase()).toString()
            prevUnd = false
        }
        i = i + 1
    }
    var start: Int = 0
    while ((start < out.length) && (out.substring(start, start + 1) == "_")) {
        start = start + 1
    }
    var end: Int = out.length
    while ((end > start) && (out.substring(end - 1, end) == "_")) {
        end = end - 1
    }
    out = out.substring(start, end)
    var res: String = ""
    var j: Int = 0
    var lastUnd: Boolean = false
    while (j < out.length) {
        var c: String = out.substring(j, j + 1)
        if (c == "_") {
            if (!lastUnd) {
                res = res + c
            }
            lastUnd = true
        } else {
            res = res + c
            lastUnd = false
        }
        j = j + 1
    }
    return res
}

fun user_main(): Unit {
    var samples: MutableList<String> = mutableListOf("snakeCase", "snake_case", "snake-case", "snake case", "snake CASE", "snake.case", "variable_10_case", "variable10Case", "ɛrgo rE tHis", "hurry-up-joe!", "c://my-docs/happy_Flag-Day/12.doc", " spaces ")
    println("=== To snake_case ===")
    for (s in samples) {
        println((padLeft(s, 34) + " => ") + camelToSnake(s))
    }
    println("")
    println("=== To camelCase ===")
    for (s in samples) {
        println((padLeft(s, 34) + " => ") + snakeToCamel(s))
    }
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
