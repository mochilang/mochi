var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

fun fields(s: String): MutableList<String> {
    var words: MutableList<String> = mutableListOf()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        val ch: String = s.substring(i, i + 1)
        if ((((ch == " ") || (ch == "\n") as Boolean)) || (ch == "\t")) {
            if (cur.length > 0) {
                words = run { val _tmp = words.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
                cur = ""
            }
        } else {
            cur = cur + ch
        }
        i = i + 1
    }
    if (cur.length > 0) {
        words = run { val _tmp = words.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    }
    return words
}

fun padRight(s: String, width: Int): String {
    var out: String = s
    var i: Int = s.length
    while (i < width) {
        out = out + " "
        i = i + 1
    }
    return out
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]
        i = i + 1
    }
    return res
}

fun validate(commands: MutableList<String>, words: MutableList<String>, mins: MutableList<Int>): MutableList<String> {
    var results: MutableList<String> = mutableListOf()
    if (words.size == 0) {
        return results
    }
    var wi: Int = 0
    while (wi < words.size) {
        val w: String = words[wi]
        var found: Boolean = false
        val wlen: Int = w.length
        var ci: Int = 0
        while (ci < commands.size) {
            val cmd: String = commands[ci]
            if ((((mins[ci] != 0) && (wlen >= mins[ci]) as Boolean)) && (wlen <= cmd.length)) {
                val c: String = cmd.toUpperCase()
                val ww: String = w.toUpperCase()
                if (c.substring(0, wlen) == ww) {
                    results = run { val _tmp = results.toMutableList(); _tmp.add(c); _tmp } as MutableList<String>
                    found = true
                    break
                }
            }
            ci = ci + 1
        }
        if (!found) {
            results = run { val _tmp = results.toMutableList(); _tmp.add("*error*"); _tmp } as MutableList<String>
        }
        wi = wi + 1
    }
    return results
}

fun user_main(): Unit {
    val table: String = ((((("Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy " + "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find ") + "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput ") + " Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO ") + "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT ") + "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT ") + "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp "
    val commands: MutableList<String> = fields(table)
    var mins: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < commands.size) {
        var count: Int = 0
        var j: Int = 0
        val cmd: String = commands[i]
        while (j < cmd.length) {
            val ch: String = cmd.substring(j, j + 1)
            if ((ch >= "A") && (ch <= "Z")) {
                count = count + 1
            }
            j = j + 1
        }
        mins = run { val _tmp = mins.toMutableList(); _tmp.add(count); _tmp } as MutableList<Int>
        i = i + 1
    }
    val sentence: String = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
    val words: MutableList<String> = fields(sentence)
    val results: MutableList<String> = validate(commands, words, mins)
    var out1: String = "user words:  "
    var k: Int = 0
    while (k < words.size) {
        out1 = (out1 + padRight(words[k], results[k].length)) + " "
        k = k + 1
    }
    println(out1)
    println("full words:  " + join(results, " "))
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
