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

data class HailResult(var seq: Int, var cnt: Int, var out: String)
var jobs: Int = 12
fun pad(n: Int): String {
    var s: String = n.toString()
    while (s.length < 4) {
        s = " " + s
    }
    return s
}

fun hail(seq: Int, cnt: Int): HailResult {
    var cnt: Int = cnt
    var seq: Int = seq
    var out: String = pad(seq)
    if (seq != 1) {
        cnt = cnt + 1
        if ((Math.floorMod(seq, 2)) != 0) {
            seq = (3 * seq) + 1
        } else {
            seq = seq / 2
        }
    }
    return HailResult(seq = seq, cnt = cnt, out = out)
}

fun user_main(): Unit {
    var seqs: MutableList<Int> = mutableListOf<Int>()
    var cnts: MutableList<Int> = mutableListOf<Int>()
    for (i in 0 until jobs) {
        seqs = run { val _tmp = seqs.toMutableList(); _tmp.add(i + 1); _tmp } as MutableList<Int>
        cnts = run { val _tmp = cnts.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
    }
    while (true) {
        var line: String = ""
        var i: Int = 0
        while (i < jobs) {
            var res: HailResult = hail(seqs[i]!!, cnts[i]!!)
            (seqs[i]) = res.seq
            (cnts[i]) = res.cnt
            line = line + res.out
            i = i + 1
        }
        println(line)
        var done: Boolean = true
        var j: Int = 0
        while (j < jobs) {
            if (seqs[j]!! != 1) {
                done = false
            }
            j = j + 1
        }
        if (done as Boolean) {
            break
        }
    }
    println("")
    println("COUNTS:")
    var counts: String = ""
    var k: Int = 0
    while (k < jobs) {
        counts = counts + pad(cnts[k]!!)
        k = k + 1
    }
    println(counts)
    println("")
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
