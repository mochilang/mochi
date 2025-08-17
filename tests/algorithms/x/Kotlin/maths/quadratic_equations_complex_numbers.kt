fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class Complex(var re: Double = 0.0, var im: Double = 0.0)
fun add(a: Complex, b: Complex): Complex {
    return Complex(re = a.re + b.re, im = a.im + b.im)
}

fun sub(a: Complex, b: Complex): Complex {
    return Complex(re = a.re - b.re, im = a.im - b.im)
}

fun div_real(a: Complex, r: Double): Complex {
    return Complex(re = a.re / r, im = a.im / r)
}

fun sqrt_newton(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x / 2.0
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun sqrt_to_complex(d: Double): Complex {
    if (d >= 0.0) {
        return Complex(re = sqrt_newton(d), im = 0.0)
    }
    return Complex(re = 0.0, im = sqrt_newton(0.0 - d))
}

fun quadratic_roots(a: Double, b: Double, c: Double): MutableList<Complex> {
    if (a == 0.0) {
        println("ValueError: coefficient 'a' must not be zero")
        return mutableListOf<Complex>()
    }
    var delta: Double = (b * b) - ((4.0 * a) * c)
    var sqrt_d: Complex = sqrt_to_complex(delta)
    var minus_b: Complex = Complex(re = 0.0 - b, im = 0.0)
    var two_a: Double = 2.0 * a
    var root1: Complex = div_real(add(minus_b, sqrt_d), two_a)
    var root2: Complex = div_real(sub(minus_b, sqrt_d), two_a)
    return mutableListOf(root1, root2)
}

fun root_str(r: Complex): String {
    if (r.im == 0.0) {
        return _numToStr(r.re)
    }
    var s: String = _numToStr(r.re)
    if (r.im >= 0.0) {
        s = ((s + "+") + _numToStr(r.im)) + "i"
    } else {
        s = (s + _numToStr(r.im)) + "i"
    }
    return s
}

fun user_main(): Unit {
    var roots: MutableList<Complex> = quadratic_roots(5.0, 6.0, 1.0)
    if (roots.size == 2) {
        println((("The solutions are: " + root_str(roots[0]!!)) + " and ") + root_str(roots[1]!!))
    }
}

fun main() {
    user_main()
}
