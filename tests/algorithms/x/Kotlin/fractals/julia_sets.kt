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

data class Complex(var re: Double = 0.0, var im: Double = 0.0)
fun complex_add(a: Complex, b: Complex): Complex {
    return Complex(re = a.re + b.re, im = a.im + b.im)
}

fun complex_mul(a: Complex, b: Complex): Complex {
    var real: Double = (a.re * b.re) - (a.im * b.im)
    var imag: Double = (a.re * b.im) + (a.im * b.re)
    return Complex(re = real, im = imag)
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun complex_abs(a: Complex): Double {
    return sqrtApprox((a.re * a.re) + (a.im * a.im))
}

fun sin_taylor(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var i: Int = (1).toInt()
    while (i < 10) {
        var k1: Double = 2.0 * ((i.toDouble()))
        var k2: Double = (2.0 * ((i.toDouble()))) + 1.0
        term = (((0.0 - term) * x) * x) / (k1 * k2)
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun cos_taylor(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Int = (1).toInt()
    while (i < 10) {
        var k1: Double = (2.0 * ((i.toDouble()))) - 1.0
        var k2: Double = 2.0 * ((i.toDouble()))
        term = (((0.0 - term) * x) * x) / (k1 * k2)
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun exp_taylor(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Double = 1.0
    while (i < 20.0) {
        term = (term * x) / i
        sum = sum + term
        i = i + 1.0
    }
    return sum
}

fun complex_exp(z: Complex): Complex {
    var e: Double = exp_taylor(z.re)
    return Complex(re = e * cos_taylor(z.im), im = e * sin_taylor(z.im))
}

fun eval_quadratic(c: Complex, z: Complex): Complex {
    return complex_add(complex_mul(z, z), c)
}

fun eval_exponential(c: Complex, z: Complex): Complex {
    return complex_add(complex_exp(z), c)
}

fun iterate_function(eval_function: (Complex, Complex) -> Complex, c: Complex, nb_iterations: Int, z0: Complex, infinity: Double): Complex {
    var z_n: Complex = z0
    var i: Int = (0).toInt()
    while (i < nb_iterations) {
        z_n = ((eval_function(c, z_n)) as Complex)
        if (complex_abs(z_n) > infinity) {
            return z_n
        }
        i = i + 1
    }
    return z_n
}

fun prepare_grid(window_size: Double, nb_pixels: Int): MutableList<MutableList<Complex>> {
    var grid: MutableList<MutableList<Complex>> = mutableListOf<MutableList<Complex>>()
    var i: Int = (0).toInt()
    while (i < nb_pixels) {
        var row: MutableList<Complex> = mutableListOf<Complex>()
        var j: Int = (0).toInt()
        while (j < nb_pixels) {
            var real: Double = (0.0 - window_size) + (((2.0 * window_size) * ((i.toDouble()))) / (((nb_pixels - 1).toDouble())))
            var imag: Double = (0.0 - window_size) + (((2.0 * window_size) * ((j.toDouble()))) / (((nb_pixels - 1).toDouble())))
            row = run { val _tmp = row.toMutableList(); _tmp.add(Complex(re = real, im = imag)); _tmp }
            j = j + 1
        }
        grid = run { val _tmp = grid.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return grid
}

fun julia_demo(): Unit {
    var grid: MutableList<MutableList<Complex>> = prepare_grid(1.0, 5)
    var c_poly: Complex = Complex(re = 0.0 - 0.4, im = 0.6)
    var c_exp: Complex = Complex(re = 0.0 - 2.0, im = 0.0)
    var poly_result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var exp_result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var y: Int = (0).toInt()
    while (y < grid.size) {
        var row_poly: MutableList<Int> = mutableListOf<Int>()
        var row_exp: MutableList<Int> = mutableListOf<Int>()
        var x: Int = (0).toInt()
        while (x < (grid[y]!!).size) {
            var z0: Complex = (((grid[y]!!) as MutableList<Complex>))[x]!!
            var z_poly: Complex = iterate_function(::eval_quadratic, c_poly, 20, z0, 4.0)
            var z_exp: Complex = iterate_function(::eval_exponential, c_exp, 10, z0, 10000000000.0)
            row_poly = run { val _tmp = row_poly.toMutableList(); _tmp.add((if (complex_abs(z_poly) < 2.0) 1 else 0.toInt())); _tmp }
            row_exp = run { val _tmp = row_exp.toMutableList(); _tmp.add((if (complex_abs(z_exp) < 10000.0) 1 else 0.toInt())); _tmp }
            x = x + 1
        }
        poly_result = run { val _tmp = poly_result.toMutableList(); _tmp.add(row_poly); _tmp }
        exp_result = run { val _tmp = exp_result.toMutableList(); _tmp.add(row_exp); _tmp }
        y = y + 1
    }
    println(poly_result)
    println(exp_result)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        julia_demo()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
