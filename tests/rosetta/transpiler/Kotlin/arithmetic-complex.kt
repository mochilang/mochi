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

data class Complex(var re: Double, var im: Double)
val a: Complex = Complex(re = 1.0, im = 1.0)
val b: Complex = Complex(re = 3.14159, im = 1.25)
fun add(a: Complex, b: Complex): Complex {
    return Complex(re = a.re + b.re, im = a.im + b.im)
}

fun mul(a: Complex, b: Complex): Complex {
    return Complex(re = (a.re * b.re) - (a.im * b.im), im = (a.re * b.im) + (a.im * b.re))
}

fun neg(a: Complex): Complex {
    return Complex(re = 0.0 - a.re, im = 0.0 - a.im)
}

fun inv(a: Complex): Complex {
    val denom: Double = (a.re * a.re) + (a.im * a.im)
    return Complex(re = a.re / denom, im = (0.0 - a.im) / denom)
}

fun conj(a: Complex): Complex {
    return Complex(re = a.re, im = 0.0 - a.im)
}

fun cstr(a: Complex): String {
    var s: String = "(" + a.re.toString()
    if (a.im >= 0) {
        s = ((s + "+") + a.im.toString()) + "i)"
    } else {
        s = (s + a.im.toString()) + "i)"
    }
    return s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("a:       " + cstr(a))
        println("b:       " + cstr(b))
        println("a + b:   " + cstr(add(a, b)))
        println("a * b:   " + cstr(mul(a, b)))
        println("-a:      " + cstr(neg(a)))
        println("1 / a:   " + cstr(inv(a)))
        println("a̅:       " + cstr(conj(a)))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
