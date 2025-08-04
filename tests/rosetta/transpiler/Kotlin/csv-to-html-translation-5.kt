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

var c: String = (((("Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n") + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") + "The multitude,Who are you?\n") + "Brians mother,I'm his mother; that's who!\n") + "The multitude,Behold his mother! Behold his mother!"
var rows: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
fun split(s: String, sep: String): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var start: Int = 0
    var i: Int = 0
    var n: Int = sep.length
    while (i <= (s.length - n)) {
        if (s.substring(i, i + n) == sep) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(s.substring(start, i)); _tmp }
            i = i + n
            start = i
        } else {
            i = i + 1
        }
    }
    out = run { val _tmp = out.toMutableList(); _tmp.add(s.substring(start, s.length)); _tmp }
    return out
}

fun htmlEscape(s: String): String {
    var out: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == "&") {
            out = out + "&amp;"
        } else {
            if (ch == "<") {
                out = out + "&lt;"
            } else {
                if (ch == ">") {
                    out = out + "&gt;"
                } else {
                    out = out + ch
                }
            }
        }
        i = i + 1
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (line in split(c, "\n")) {
            rows = run { val _tmp = rows.toMutableList(); _tmp.add(split(line, ",")); _tmp }
        }
        println("<table>")
        for (row in rows) {
            var cells: String = ""
            for (cell in row) {
                cells = ((cells + "<td>") + htmlEscape(cell)) + "</td>"
            }
            println(("    <tr>" + cells) + "</tr>")
        }
        println("</table>")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
