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

fun bellTriangle(n: Int): MutableList<MutableList<BigInteger>> {
    var tri: MutableList<MutableList<BigInteger>> = mutableListOf<MutableList<BigInteger>>()
    var i: Int = 0
    while (i < n) {
        var row: MutableList<BigInteger> = mutableListOf<BigInteger>()
        var j: Int = 0
        while (j < i) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.toBigInteger()); _tmp } as MutableList<BigInteger>
            j = j + 1
        }
        tri = run { val _tmp = tri.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<BigInteger>>
        i = i + 1
    }
    tri[1][0] = 1.toBigInteger()
    i = 2
    while (i < n) {
        tri[i][0] = tri[i - 1][i - 2]
        var j: Int = 1
        while (j < i) {
            tri[i][j] = tri[i][j - 1].add(tri[i - 1][j - 1])
            j = j + 1
        }
        i = i + 1
    }
    return tri
}

fun user_main(): Unit {
    val bt: MutableList<MutableList<BigInteger>> = bellTriangle(51)
    println("First fifteen and fiftieth Bell numbers:")
    for (i in 1 until 16) {
        println((("" + (i.toString().padStart(2, " "[0])).toString()) + ": ") + (bt[i][0]).toString())
    }
    println("50: " + (bt[50][0]).toString())
    println("")
    println("The first ten rows of Bell's triangle:")
    for (i in 1 until 11) {
        println(bt[i])
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
