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

var s4: MutableList<Int> = mutableListOf(4, 3, 3, 3, 1, 2, 0, 2, 3)
var s1: MutableList<Int> = mutableListOf(1, 2, 0, 2, 1, 1, 0, 1, 3)
var s2: MutableList<Int> = mutableListOf(2, 1, 3, 1, 0, 1, 0, 1, 0)
var s3_a: MutableList<Int> = plus(s1, s2)
var s3_b: MutableList<Int> = plus(s2, s1)
var s3: MutableList<Int> = mutableListOf(3, 3, 3, 3, 3, 3, 3, 3, 3)
var s3_id: MutableList<Int> = mutableListOf(2, 1, 2, 1, 0, 1, 2, 1, 2)
var s4b: MutableList<Int> = plus(s3, s3_id)
var s5: MutableList<Int> = plus(s3_id, s3_id)
fun neighborsList(): MutableList<MutableList<Int>> {
    return mutableListOf(mutableListOf(1, 3), mutableListOf(0, 2, 4), mutableListOf(1, 5), mutableListOf(0, 4, 6), mutableListOf(1, 3, 5, 7), mutableListOf(2, 4, 8), mutableListOf(3, 7), mutableListOf(4, 6, 8), mutableListOf(5, 7))
}

fun plus(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < a.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(a[i] + b[i]); _tmp } as MutableList<Int>
        i = i + 1
    }
    return res
}

fun isStable(p: MutableList<Int>): Boolean {
    for (v in p) {
        if (v > 3) {
            return false
        }
    }
    return true
}

fun topple(p: MutableList<Int>): Int {
    val neighbors: MutableList<MutableList<Int>> = neighborsList()
    var i: Int = 0
    while (i < p.size) {
        if (p[i] > 3) {
            p[i] = p[i] - 4
            val nbs: MutableList<Int> = neighbors[i]
            for (j in nbs) {
                p[j] = p[j] + 1
            }
            return 0
        }
        i = i + 1
    }
    return 0
}

fun pileString(p: MutableList<Int>): String {
    var s: String = ""
    var r: Int = 0
    while (r < 3) {
        var c: Int = 0
        while (c < 3) {
            s = (s + (p[(3 * r) + c]).toString()) + " "
            c = c + 1
        }
        s = s + "\n"
        r = r + 1
    }
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Avalanche of topplings:\n")
        println(pileString(s4))
        while (!isStable(s4)) {
            topple(s4)
            println(pileString(s4))
        }
        println("Commutative additions:\n")
        while (!isStable(s3_a)) {
            topple(s3_a)
        }
        while (!isStable(s3_b)) {
            topple(s3_b)
        }
        println((((pileString(s1) + "\nplus\n\n") + pileString(s2)) + "\nequals\n\n") + pileString(s3_a))
        println((((("and\n\n" + pileString(s2)) + "\nplus\n\n") + pileString(s1)) + "\nalso equals\n\n") + pileString(s3_b))
        println("Addition of identity sandpile:\n")
        while (!isStable(s4b)) {
            topple(s4b)
        }
        println((((pileString(s3) + "\nplus\n\n") + pileString(s3_id)) + "\nequals\n\n") + pileString(s4b))
        println("Addition of identities:\n")
        while (!isStable(s5)) {
            topple(s5)
        }
        println((((pileString(s3_id) + "\nplus\n\n") + pileString(s3_id)) + "\nequals\n\n") + pileString(s5))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
