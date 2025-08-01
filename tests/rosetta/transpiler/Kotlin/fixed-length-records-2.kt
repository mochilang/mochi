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

var lines: MutableList<String> = mutableListOf("alpha", "beta", "gamma")
var blocks: MutableList<String> = text2block(lines)
var outLines: MutableList<String> = block2text(blocks)
fun repeat(s: String, n: Int): String {
    var out: String = ""
    var i: Int = 0
    while (i < n) {
        out = out + s
        i = i + 1
    }
    return out
}

fun trimRightSpace(s: String): String {
    var i: BigInteger = (s.length - 1).toBigInteger()
    while ((i.compareTo(0.toBigInteger()) >= 0) && (s.substring((i).toInt(), (i.add(1.toBigInteger())).toInt()) == " ")) {
        i = i.subtract(1.toBigInteger())
    }
    return s.substring(0, (i.add(1.toBigInteger())).toInt())
}

fun block2text(block: MutableList<String>): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    for (b in block) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(trimRightSpace(b)); _tmp } as MutableList<String>
    }
    return out
}

fun text2block(lines: MutableList<String>): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var count: Int = 0
    for (line in lines) {
        var s: String = line
        var le: Int = s.length
        if (le > 64) {
            s = s.substring(0, 64)
        } else {
            if (le < 64) {
                s = s + repeat(" ", 64 - le)
            }
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(s); _tmp } as MutableList<String>
        count = count + 1
    }
    if ((Math.floorMod(count, 16)) != 0) {
        var pad: Int = 16 - (Math.floorMod(count, 16))
        var i: Int = 0
        while (i < pad) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(repeat(" ", 64)); _tmp } as MutableList<String>
            i = i + 1
        }
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (l in outLines) {
            if (l != "") {
                println(l)
            }
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
