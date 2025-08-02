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

typealias Fn = (Any?) -> Any?
typealias Church = ((Any?) -> Any?) -> (Any?) -> Any?
var z: Church = ::zero
var three = succ((succ((succ(z)) as ((Any?) -> Any?) -> (Any?) -> Any?)) as ((Any?) -> Any?) -> (Any?) -> Any?)
var four = succ(three)
fun zero(f: Fn): Fn {
    return { x: Any? -> x } as Fn
}

fun succ(c: Church): Church {
    return { f: Fn -> { x: Any? -> (f(c(f)(x) as Any?)) as Any? } as Fn } as Church
}

fun add(c: Church, d: Church): Church {
    return { f: Fn -> { x: Any? -> c(f)(d(f)(x)) as Any? } as Fn } as Church
}

fun mul(c: Church, d: Church): Church {
    return { f: Fn -> { x: Any? -> c((d(f)) as (Any?) -> Any?)(x) as Any? } as Fn } as Church
}

fun pow(c: Church, d: Church): Church {
    var di: Int = toInt(d)
    var prod: ((Any?) -> Any?) -> (Any?) -> Any? = c
    var i: Int = 1
    while (i < di) {
        prod = mul(prod, c)
        i = i + 1
    }
    return prod as Church
}

fun incr(i: Any?): Any? {
    return ((i as Int) + 1) as Any?
}

fun toInt(c: Church): Int {
    return c(::incr)(0) as Int
}

fun intToChurch(i: Int): Church {
    if (i == 0) {
        return ::zero as Church
    }
    return succ((intToChurch(i - 1)) as ((Any?) -> Any?) -> (Any?) -> Any?)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("three        -> " + toInt(three).toString())
        println("four         -> " + toInt(four).toString())
        println("three + four -> " + toInt((add(three, four)) as ((Any?) -> Any?) -> (Any?) -> Any?).toString())
        println("three * four -> " + toInt((mul(three, four)) as ((Any?) -> Any?) -> (Any?) -> Any?).toString())
        println("three ^ four -> " + toInt((pow(three, four)) as ((Any?) -> Any?) -> (Any?) -> Any?).toString())
        println("four ^ three -> " + toInt((pow(four, three)) as ((Any?) -> Any?) -> (Any?) -> Any?).toString())
        println("5 -> five    -> " + toInt((intToChurch(5)) as ((Any?) -> Any?) -> (Any?) -> Any?).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
