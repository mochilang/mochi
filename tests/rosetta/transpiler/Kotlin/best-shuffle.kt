import java.math.BigInteger

fun _len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> v.toString().length
}

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

fun nextRand(seed: Int): Int {
    return Math.floorMod(((seed * 1664525) + 1013904223), 2147483647)
}

fun shuffleChars(s: String, seed: Int): MutableList<Any?> {
    var chars: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < s.length) {
        chars = run { val _tmp = chars.toMutableList(); _tmp.add(s.substring(i, i + 1)); _tmp } as MutableList<String>
        i = i + 1
    }
    var sd: Int = seed
    var idx: BigInteger = (chars.size - 1).toBigInteger()
    while (idx.compareTo((0).toBigInteger()) > 0) {
        sd = nextRand(sd)
        var j: BigInteger = (sd).toBigInteger().remainder((idx.add((1).toBigInteger())))
        var tmp: String = chars[(idx).toInt()]!!
        chars[(idx).toInt()] = chars[(j).toInt()]!!
        chars[(j).toInt()] = tmp
        idx = idx.subtract((1).toBigInteger())
    }
    var res: String = ""
    i = 0
    while (i < chars.size) {
        res = res + chars[i]!!
        i = i + 1
    }
    return mutableListOf<Any?>(res as Any?, sd as Any?)
}

fun bestShuffle(s: String, seed: Int): MutableList<Any?> {
    var r: MutableList<Any?> = shuffleChars(s, seed)
    var t: Any? = r[0] as Any?
    var sd: Any? = r[1] as Any?
    var arr: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < _len(t)) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add((t).toString().substring(i, i + 1)); _tmp } as MutableList<String>
        i = i + 1
    }
    i = 0
    while (i < arr.size) {
        var j: Int = 0
        while (j < arr.size) {
            if ((((i != j) && (arr[i]!! != s.substring(j, j + 1)) as Boolean)) && (arr[j]!! != s.substring(i, i + 1))) {
                var tmp: String = arr[i]!!
                arr[i] = arr[j]!!
                arr[j] = tmp
                break
            }
            j = j + 1
        }
        i = i + 1
    }
    var count: Int = 0
    i = 0
    while (i < arr.size) {
        if (arr[i]!! == s.substring(i, i + 1)) {
            count = count + 1
        }
        i = i + 1
    }
    var out: String = ""
    i = 0
    while (i < arr.size) {
        out = out + arr[i]!!
        i = i + 1
    }
    return mutableListOf<Any?>(out as Any?, sd, count as Any?)
}

fun user_main(): Unit {
    var ts: MutableList<String> = mutableListOf("abracadabra", "seesaw", "elk", "grrrrrr", "up", "a")
    var seed: Int = 1
    var i: Int = 0
    while (i < ts.size) {
        var r: MutableList<Any?> = bestShuffle(ts[i]!!, seed)
        var shuf: Any? = r[0] as Any?
        seed = (r[1] as Any?) as Int
        var cnt: Any? = r[2] as Any?
        println(((((ts[i]!! + " -> ") + (shuf).toString()) + " (") + cnt.toString()) + ")")
        i = i + 1
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
