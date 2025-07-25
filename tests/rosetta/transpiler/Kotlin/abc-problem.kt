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
    var res: MutableList<String> = mutableListOf()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        val c: String = s.substring(i, i + 1)
        if (c == " ") {
            if (cur.length > 0) {
                res = run { val _tmp = res.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
                cur = ""
            }
        } else {
            cur = cur + c
        }
        i = i + 1
    }
    if (cur.length > 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    }
    return res
}

fun canSpell(word: String, blks: MutableList<String>): Boolean {
    if (word.length == 0) {
        return true
    }
    val c: String = word.substring(0, 1).toLowerCase()
    var i: Int = 0
    while (i < blks.size) {
        val b: String = blks[i]
        if ((c == b.substring(0, 1).toLowerCase()) || (c == b.substring(1, 2).toLowerCase())) {
            var rest: MutableList<String> = mutableListOf()
            var j: Int = 0
            while (j < blks.size) {
                if (j != i) {
                    rest = run { val _tmp = rest.toMutableList(); _tmp.add(blks[j]); _tmp } as MutableList<String>
                }
                j = j + 1
            }
            if ((canSpell(word.substring(1, word.length), rest)) as Boolean) {
                return true
            }
        }
        i = i + 1
    }
    return false
}

fun newSpeller(blocks: String): (String) -> Boolean {
    val bl: MutableList<String> = fields(blocks)
    return { w: String -> canSpell(w, bl) } as (String) -> Boolean
}

fun user_main(): Unit {
    val sp: (String) -> Boolean = newSpeller("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM")
    for (word in mutableListOf("A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE")) {
        println((word + " ") + sp(word).toString())
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
