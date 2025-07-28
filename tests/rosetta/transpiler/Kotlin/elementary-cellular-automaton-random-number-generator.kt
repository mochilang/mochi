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

val n: Int = 64
var init: MutableList<Int> = mutableListOf<Int>()
var i: Int = 0
val bytes: MutableList<Int> = evolve(init, 30)
fun pow2(k: Int): Int {
    var v: Int = 1
    var i: Int = 0
    while (i < k) {
        v = v * 2
        i = i + 1
    }
    return v
}

fun ruleBit(ruleNum: Int, idx: Int): Int {
    var r: Int = ruleNum
    var i: Int = 0
    while (i < idx) {
        r = r / 2
        i = i + 1
    }
    return Math.floorMod(r, 2)
}

fun evolve(state: MutableList<Int>, ruleNum: Int): MutableList<Int> {
    var state: MutableList<Int> = state
    var out: MutableList<Int> = mutableListOf<Int>()
    var p: Int = 0
    while (p < 10) {
        var b: Int = 0
        var q: Int = 7
        while (q >= 0) {
            val st: MutableList<Int> = state
            b = b + (st[0] * pow2(q))
            var next: MutableList<Int> = mutableListOf<Int>()
            var i: Int = 0
            while (i < n) {
                var lidx: BigInteger = (i - 1).toBigInteger()
                if (lidx.compareTo(0.toBigInteger()) < 0) {
                    lidx = (n - 1).toBigInteger()
                }
                val left: Int = st[(lidx).toInt()]
                val center: Int = st[i]
                val ridx: BigInteger = (i + 1).toBigInteger()
                if (ridx.compareTo(n.toBigInteger()) >= 0) {
                    ridx = 0.toBigInteger()
                }
                val right: Int = st[(ridx).toInt()]
                val index: BigInteger = (((left * 4) + (center * 2)) + right).toBigInteger()
                next = run { val _tmp = next.toMutableList(); _tmp.add(ruleBit(ruleNum, index.toInt())); _tmp } as MutableList<Int>
                i = i + 1
            }
            state = next
            q = q - 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(b); _tmp } as MutableList<Int>
        p = p + 1
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < n) {
            init = run { val _tmp = init.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
            i = i + 1
        }
        init[0] = 1
        println(bytes.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
