import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class Polynomial(var degree: Int = 0, var coefficients: MutableList<Double> = mutableListOf<Double>())
fun copy_list(xs: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun polynomial_new(degree: Int, coeffs: MutableList<Double>): Polynomial {
    if (coeffs.size != (degree + 1)) {
        panic("The number of coefficients should be equal to the degree + 1.")
    }
    return Polynomial(degree = degree, coefficients = copy_list(coeffs))
}

fun add(p: Polynomial, q: Polynomial): Polynomial {
    if (p.degree > q.degree) {
        var coeffs: MutableList<Double> = copy_list(p.coefficients)
        var i: Int = (0).toInt()
        while (i <= q.degree) {
            _listSet(coeffs, i, coeffs[i]!! + (q.coefficients)[i]!!)
            i = i + 1
        }
        return Polynomial(degree = p.degree, coefficients = coeffs)
    } else {
        var coeffs: MutableList<Double> = copy_list(q.coefficients)
        var i: Int = (0).toInt()
        while (i <= p.degree) {
            _listSet(coeffs, i, coeffs[i]!! + (p.coefficients)[i]!!)
            i = i + 1
        }
        return Polynomial(degree = q.degree, coefficients = coeffs)
    }
}

fun neg(p: Polynomial): Polynomial {
    var coeffs: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i <= p.degree) {
        coeffs = run { val _tmp = coeffs.toMutableList(); _tmp.add(0.0 - (p.coefficients)[i]!!); _tmp }
        i = i + 1
    }
    return Polynomial(degree = p.degree, coefficients = coeffs)
}

fun sub(p: Polynomial, q: Polynomial): Polynomial {
    return add(p, neg(q))
}

fun mul(p: Polynomial, q: Polynomial): Polynomial {
    var size: Int = ((p.degree + q.degree) + 1).toInt()
    var coeffs: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < size) {
        coeffs = run { val _tmp = coeffs.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    i = 0
    while (i <= p.degree) {
        var j: Int = (0).toInt()
        while (j <= q.degree) {
            _listSet(coeffs, i + j, coeffs[i + j]!! + ((p.coefficients)[i]!! * (q.coefficients)[j]!!))
            j = j + 1
        }
        i = i + 1
    }
    return Polynomial(degree = p.degree + q.degree, coefficients = coeffs)
}

fun power(base: Double, exp: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun evaluate(p: Polynomial, x: Double): Double {
    var result: Double = 0.0
    var i: Int = (0).toInt()
    while (i <= p.degree) {
        result = result + ((p.coefficients)[i]!! * power(x, i))
        i = i + 1
    }
    return result
}

fun poly_to_string(p: Polynomial): String {
    var s: String = ""
    var i: Int = (p.degree).toInt()
    while (i >= 0) {
        var coeff: Double = (p.coefficients)[i]!!
        if (coeff != 0.0) {
            if (s.length > 0) {
                if (coeff > 0.0) {
                    s = s + " + "
                } else {
                    s = s + " - "
                }
            } else {
                if (coeff < 0.0) {
                    s = s + "-"
                }
            }
            var abs_coeff: Double = if (coeff < 0.0) 0.0 - coeff else coeff.toDouble()
            if (i == 0) {
                s = s + _numToStr(abs_coeff)
            } else {
                if (i == 1) {
                    s = (s + _numToStr(abs_coeff)) + "x"
                } else {
                    s = ((s + _numToStr(abs_coeff)) + "x^") + _numToStr(i)
                }
            }
        }
        i = i - 1
    }
    if (s == "") {
        s = "0"
    }
    return s
}

fun derivative(p: Polynomial): Polynomial {
    if (p.degree == 0) {
        return Polynomial(degree = 0, coefficients = mutableListOf(0.0))
    }
    var coeffs: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < p.degree) {
        coeffs = run { val _tmp = coeffs.toMutableList(); _tmp.add((p.coefficients)[i + 1]!! * ((i + 1).toDouble())); _tmp }
        i = i + 1
    }
    return Polynomial(degree = p.degree - 1, coefficients = coeffs)
}

fun integral(p: Polynomial, constant: Double): Polynomial {
    var coeffs: MutableList<Double> = mutableListOf(constant)
    var i: Int = (0).toInt()
    while (i <= p.degree) {
        coeffs = run { val _tmp = coeffs.toMutableList(); _tmp.add((p.coefficients)[i]!! / ((i + 1).toDouble())); _tmp }
        i = i + 1
    }
    return Polynomial(degree = p.degree + 1, coefficients = coeffs)
}

fun equals(p: Polynomial, q: Polynomial): Boolean {
    if (p.degree != q.degree) {
        return false
    }
    var i: Int = (0).toInt()
    while (i <= p.degree) {
        if ((p.coefficients)[i]!! != (q.coefficients)[i]!!) {
            return false
        }
        i = i + 1
    }
    return true
}

fun not_equals(p: Polynomial, q: Polynomial): Boolean {
    return !equals(p, q) as Boolean
}

fun test_polynomial(): Unit {
    var p: Polynomial = polynomial_new(2, mutableListOf(1.0, 2.0, 3.0))
    var q: Polynomial = polynomial_new(2, mutableListOf(1.0, 2.0, 3.0))
    if (poly_to_string(add(p, q)) != "6x^2 + 4x + 2") {
        panic("add failed")
    }
    if (poly_to_string(sub(p, q)) != "0") {
        panic("sub failed")
    }
    if (evaluate(p, 2.0) != 17.0) {
        panic("evaluate failed")
    }
    if (poly_to_string(derivative(p)) != "6x + 2") {
        panic("derivative failed")
    }
    var integ: String = poly_to_string(integral(p, 0.0))
    if (integ != "1x^3 + 1x^2 + 1x") {
        panic("integral failed")
    }
    if (!equals(p, q)) {
        panic("equals failed")
    }
    if ((not_equals(p, q)) as Boolean) {
        panic("not_equals failed")
    }
}

fun user_main(): Unit {
    test_polynomial()
    var p: Polynomial = polynomial_new(2, mutableListOf(1.0, 2.0, 3.0))
    var d: Polynomial = derivative(p)
    println(poly_to_string(d))
}

fun main() {
    user_main()
}
