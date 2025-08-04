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

typealias Church = ((Any?) -> Any?) -> (Any?) -> Any?
fun id(x: Any?): Any? {
    return x
}

fun compose(f: (Any?) -> Any?, g: (Any?) -> Any?): (Any?) -> Any? {
    return ({ x: Any? -> ((f(((g(x)) as Any?))) as Any?) } as (Any?) -> Any?)
}

fun zero(): Church {
    return ({ f: Church -> (::id as Church) } as Church)
}

fun one(): Church {
    return (::id as Church)
}

fun succ(n: Church): Church {
    return ({ f: Church -> ((compose(f, ((n((f as Any?))) as (Any?) -> Any?))) as Church) } as Church)
}

fun plus(m: Church, n: Church): Church {
    return ({ f: Church -> ((compose(((m((f as Any?))) as (Any?) -> Any?), ((n((f as Any?))) as (Any?) -> Any?))) as Church) } as Church)
}

fun mult(m: Church, n: Church): Church {
    return ((compose(m, n)) as Church)
}

fun exp(m: Church, n: Church): Church {
    return ((n((m as Any?))) as Church)
}

fun toInt(x: Church): Int {
    var counter: Int = 0
    fun fCounter(f: Church): Church {
        counter = counter + 1
        return (f as Church)
    }

    x((::fCounter as Any?))(::id)
    return counter
}

fun toStr(x: Church): String {
    var s: String = ""
    fun fCounter(f: Church): Church {
        s = s + "|"
        return (f as Church)
    }

    x((::fCounter as Any?))(::id)
    return s
}

fun user_main(): Unit {
    println("zero = " + toInt(((zero()) as (Any?) -> Any?)).toString())
    var onev: Church = one()
    println("one = " + toInt(onev).toString())
    var two: Church = succ(((succ(((zero()) as (Any?) -> Any?))) as (Any?) -> Any?))
    println("two = " + toInt(two).toString())
    var three: Church = plus(onev, two)
    println("three = " + toInt(three).toString())
    var four: Church = mult(two, two)
    println("four = " + toInt(four).toString())
    var eight: Church = exp(two, three)
    println("eight = " + toInt(eight).toString())
    println("toStr(four) = " + toStr(four))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
