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

fun contains(xs: MutableList<Int>, n: Int): Boolean {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i] == n) {
            return true
        }
        i = i + 1
    }
    return false
}

fun gcd(a: Int, b: Int): Int {
    var x: Int = a
    var y: Int = b
    while (y != 0) {
        val t: BigInteger = (Math.floorMod(x, y)).toBigInteger()
        x = y
        y = t.toInt()
    }
    if (x < 0) {
        x = 0 - x
    }
    return x
}

fun sortInts(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = xs
    var n: Int = arr.size
    var i: Int = 0
    while (i < n) {
        var j: Int = 0
        while (j < (n - 1)) {
            if (arr[j] > arr[j + 1]) {
                val tmp: Int = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = tmp
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

fun areSame(s: MutableList<Int>, t: MutableList<Int>): Boolean {
    if (s.size != t.size) {
        return false
    }
    var a: MutableList<Int> = sortInts(s)
    var b: MutableList<Int> = sortInts(t)
    var i: Int = 0
    while (i < a.size) {
        if (a[i] != b[i]) {
            return false
        }
        i = i + 1
    }
    return true
}

fun printSlice(start: Int, seq: MutableList<Int>): Unit {
    var first: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < 30) {
        first = run { val _tmp = first.toMutableList(); _tmp.add(seq[i]); _tmp } as MutableList<Int>
        i = i + 1
    }
    var pad: String = ""
    if (start < 10) {
        pad = " "
    }
    println(((("EKG(" + pad) + start.toString()) + "): ") + first.toString())
}

fun user_main(): Unit {
    val limit: Int = 100
    val starts: MutableList<Int> = mutableListOf(2, 5, 7, 9, 10)
    var ekg: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var s: Int = 0
    while (s < starts.size) {
        var seq: MutableList<Int> = mutableListOf(1, starts[s])
        var n: Int = 2
        while (n < limit) {
            var i: Int = 2
            var done: Boolean = false
            while (!done) {
                if ((!contains(seq, i) as Boolean) && (gcd(seq[n - 1], i) > 1)) {
                    seq = run { val _tmp = seq.toMutableList(); _tmp.add(i); _tmp } as MutableList<Int>
                    done = true
                }
                i = i + 1
            }
            n = n + 1
        }
        ekg = run { val _tmp = ekg.toMutableList(); _tmp.add(seq); _tmp } as MutableList<MutableList<Int>>
        printSlice(starts[s], seq)
        s = s + 1
    }
    var i: Int = 2
    var found: Boolean = false
    while (i < limit) {
        if ((ekg[1][i] == ekg[2][i]) && areSame(ekg[1].subList(0, i), ekg[2].subList(0, i))) {
            println("\nEKG(5) and EKG(7) converge at term " + (i + 1).toString())
            found = true
            break
        }
        i = i + 1
    }
    if (!found) {
        println(("\nEKG5(5) and EKG(7) do not converge within " + limit.toString()) + " terms")
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
