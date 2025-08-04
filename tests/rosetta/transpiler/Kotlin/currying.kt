fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
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

data class Foo(var value: Int = 0) {
    fun Method(b: Int): Int {
        return (value as Int) + b
    }
}
fun pow(base: Double, exp: Double): Double {
    var result: Double = 1.0
    var i: Int = 0
    while (i < ((exp.toInt()))) {
        result = result * base
        i = i + 1
    }
    return result
}

fun PowN(b: Double): (Double) -> Double {
    return ({ e: Double -> pow(b, e) } as (Double) -> Double)
}

fun PowE(e: Double): (Double) -> Double {
    return ({ b: Double -> pow(b, e) } as (Double) -> Double)
}

fun user_main(): Unit {
    var pow2: (Double) -> Double = PowN(2.0)
    var cube: (Double) -> Double = PowE(3.0)
    println("2^8 = " + pow2(8.0).toString())
    println("4³ = " + cube(4.0).toString())
    var a: Foo = Foo(value = 2)
    var fn1 = { b: Int -> a.Method(b) }
    var fn2 = { f: Foo, b: Int -> f.Method(b) }
    println("2 + 2 = " + a.Method(2).toString())
    println("2 + 3 = " + fn1(3).toString())
    println("2 + 4 = " + fn2(a, 4).toString())
    println("3 + 5 = " + fn2(Foo(value = 3), 5).toString())
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
