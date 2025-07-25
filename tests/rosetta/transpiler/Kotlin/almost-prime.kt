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

fun kPrime(n: Int, k: Int): Boolean {
    var n: Int = n
    var nf: Int = 0
    var i: Int = 2
    while (i <= n) {
        while ((n % i) == 0) {
            if (nf == k) {
                return false
            }
            nf = nf + 1
            n = n / i
        }
        i = i + 1
    }
    return nf == k
}

fun gen(k: Int, count: Int): MutableList<Int> {
    var r: MutableList<Int> = mutableListOf()
    var n: Int = 2
    while (r.size < count) {
        if ((kPrime(n, k)) as Boolean) {
            r = run { val _tmp = r.toMutableList(); _tmp.add(n); _tmp } as MutableList<Int>
        }
        n = n + 1
    }
    return r
}

fun user_main(): Unit {
    var k: Int = 1
    while (k <= 5) {
        println((k.toString() + " ") + gen(k, 10).toString())
        k = k + 1
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
