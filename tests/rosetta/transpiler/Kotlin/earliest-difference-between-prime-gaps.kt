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

fun commatize(n: Int): String {
    var s: String = n.toString()
    var i: BigInteger = (s.length - 3).toBigInteger()
    while (i.compareTo(0.toBigInteger()) > 0) {
        s = (s.substring(0, (i).toInt()) + ",") + s.substring((i).toInt(), s.length)
        i = i.subtract(3.toBigInteger())
    }
    return s
}

fun user_main(): Unit {
    val data: MutableList<MutableMap<String, Int>> = mutableListOf(mutableMapOf<String, Int>("pm" to (10), "g1" to (4), "s1" to (7), "g2" to (6), "s2" to (23), "d" to (16)), mutableMapOf<String, Int>("pm" to (100), "g1" to (14), "s1" to (113), "g2" to (16), "s2" to (1831), "d" to (1718)), mutableMapOf<String, Int>("pm" to (1000), "g1" to (14), "s1" to (113), "g2" to (16), "s2" to (1831), "d" to (1718)), mutableMapOf<String, Int>("pm" to (10000), "g1" to (36), "s1" to (9551), "g2" to (38), "s2" to (30593), "d" to (21042)), mutableMapOf<String, Int>("pm" to (100000), "g1" to (70), "s1" to (173359), "g2" to (72), "s2" to (31397), "d" to (141962)), mutableMapOf<String, Int>("pm" to (1000000), "g1" to (100), "s1" to (396733), "g2" to (102), "s2" to (1444309), "d" to (1047576)), mutableMapOf<String, Int>("pm" to (10000000), "g1" to (148), "s1" to (2010733), "g2" to (150), "s2" to (13626257), "d" to (11615524)), mutableMapOf<String, Int>("pm" to (100000000), "g1" to (198), "s1" to (46006769), "g2" to (200), "s2" to (378043979), "d" to (332037210)), mutableMapOf<String, Any?>("pm" to (1000000000), "g1" to (276), "s1" to (649580171), "g2" to (278), "s2" to (4260928601L), "d" to (3611348430L)), mutableMapOf<String, Any?>("pm" to (10000000000L), "g1" to (332), "s1" to (5893180121L), "g2" to (334), "s2" to (30827138509L), "d" to (24933958388L)), mutableMapOf<String, Any?>("pm" to (100000000000L), "g1" to (386), "s1" to (35238645587L), "g2" to (388), "s2" to (156798792223L), "d" to (121560146636L)))
    for (entry in data) {
        val pm: String = commatize((entry)["pm"] as Int)
        val line1: String = ("Earliest difference > " + pm) + " between adjacent prime gap starting primes:"
        println(line1)
        val line2: String = ((((((((("Gap " + ((entry)["g1"] as Int).toString()) + " starts at ") + commatize((entry)["s1"] as Int)) + ", gap ") + ((entry)["g2"] as Int).toString()) + " starts at ") + commatize((entry)["s2"] as Int)) + ", difference is ") + commatize((entry)["d"] as Int)) + "."
        println(line2)
        println("")
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
