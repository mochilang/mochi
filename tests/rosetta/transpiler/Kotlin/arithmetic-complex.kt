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
    println("a:       " + cstr(a))
    println("b:       " + cstr(b))
    println("a + b:   " + cstr(add(a, b)))
    println("a * b:   " + cstr(mul(a, b)))
    println("-a:      " + cstr(neg(a)))
    println("1 / a:   " + cstr(inv(a)))
    println("a̅:       " + cstr(conj(a)))
}
