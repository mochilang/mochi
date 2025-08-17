fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

data class Complex(var re: Double = 0.0, var im: Double = 0.0)
var PI: Double = 3.141592653589793
var A: MutableList<Double> = mutableListOf(0.0, 1.0, 0.0, 2.0)
var B: MutableList<Double> = mutableListOf(2.0, 3.0, 4.0, 0.0)
var product: MutableList<Double> = multiply_poly(A, B)
fun c_add(a: Complex, b: Complex): Complex {
    return Complex(re = a.re + b.re, im = a.im + b.im)
}

fun c_sub(a: Complex, b: Complex): Complex {
    return Complex(re = a.re - b.re, im = a.im - b.im)
}

fun c_mul(a: Complex, b: Complex): Complex {
    return Complex(re = (a.re * b.re) - (a.im * b.im), im = (a.re * b.im) + (a.im * b.re))
}

fun c_mul_scalar(a: Complex, s: Double): Complex {
    return Complex(re = a.re * s, im = a.im * s)
}

fun c_div_scalar(a: Complex, s: Double): Complex {
    return Complex(re = a.re / s, im = a.im / s)
}

fun sin_taylor(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var i: Int = (1).toInt()
    while (i < 10) {
        var k1: Double = 2.0 * (i.toDouble())
        var k2: Double = k1 + 1.0
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
        var k1: Double = (2.0 * (i.toDouble())) - 1.0
        var k2: Double = 2.0 * (i.toDouble())
        term = (((0.0 - term) * x) * x) / (k1 * k2)
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun exp_i(theta: Double): Complex {
    return Complex(re = cos_taylor(theta), im = sin_taylor(theta))
}

fun make_complex_list(n: Int, value: Complex): MutableList<Complex> {
    var arr: MutableList<Complex> = mutableListOf<Complex>()
    var i: Int = (0).toInt()
    while (i < n) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(value); _tmp }
        i = i + 1
    }
    return arr
}

fun fft(a: MutableList<Complex>, invert: Boolean): MutableList<Complex> {
    var n: Int = (a.size).toInt()
    if (n == 1) {
        return mutableListOf(a[0]!!)
    }
    var a0: MutableList<Complex> = mutableListOf<Complex>()
    var a1: MutableList<Complex> = mutableListOf<Complex>()
    var i: Int = (0).toInt()
    while (i < (n / 2)) {
        a0 = run { val _tmp = a0.toMutableList(); _tmp.add(a[2 * i]!!); _tmp }
        a1 = run { val _tmp = a1.toMutableList(); _tmp.add(a[(2 * i) + 1]!!); _tmp }
        i = i + 1
    }
    var y0: MutableList<Complex> = fft(a0, invert)
    var y1: MutableList<Complex> = fft(a1, invert)
    var angle: Double = ((2.0 * PI) / (n.toDouble())) * if (invert != null) 0.0 - 1.0 else 1.0
    var w: Complex = Complex(re = 1.0, im = 0.0)
    var wn: Complex = exp_i(angle)
    var y: MutableList<Complex> = make_complex_list(n, Complex(re = 0.0, im = 0.0))
    i = 0
    while (i < (n / 2)) {
        var t: Complex = c_mul(w, y1[i]!!)
        var u: Complex = y0[i]!!
        var even: Complex = c_add(u, t)
        var odd: Complex = c_sub(u, t)
        if (invert as Boolean) {
            even = c_div_scalar(even, 2.0)
            odd = c_div_scalar(odd, 2.0)
        }
        _listSet(y, i, even)
        _listSet(y, i + (n / 2), odd)
        w = c_mul(w, wn)
        i = i + 1
    }
    return y
}

fun floor(x: Double): Double {
    var i: Int = (x.toInt()).toInt()
    if ((i.toDouble()) > x) {
        i = i - 1
    }
    return i.toDouble()
}

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        p = p * 10.0
        i = i + 1
    }
    return p
}

fun round_to(x: Double, ndigits: Int): Double {
    var m: Double = pow10(ndigits)
    return floor((x * m) + 0.5) / m
}

fun list_to_string(l: MutableList<Double>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < l.size) {
        s = s + _numToStr(l[i]!!)
        if ((i + 1) < l.size) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun multiply_poly(a: MutableList<Double>, b: MutableList<Double>): MutableList<Double> {
    var n: Int = (1).toInt()
    while (n < ((a.size + b.size) - 1)) {
        n = n * 2
    }
    var fa: MutableList<Complex> = make_complex_list(n, Complex(re = 0.0, im = 0.0))
    var fb: MutableList<Complex> = make_complex_list(n, Complex(re = 0.0, im = 0.0))
    var i: Int = (0).toInt()
    while (i < a.size) {
        _listSet(fa, i, Complex(re = a[i]!!, im = 0.0))
        i = i + 1
    }
    i = 0
    while (i < b.size) {
        _listSet(fb, i, Complex(re = b[i]!!, im = 0.0))
        i = i + 1
    }
    fa = fft(fa, false)
    fb = fft(fb, false)
    i = 0
    while (i < n) {
        _listSet(fa, i, c_mul(fa[i]!!, fb[i]!!))
        i = i + 1
    }
    fa = fft(fa, true)
    var res: MutableList<Double> = mutableListOf<Double>()
    i = 0
    while (i < ((a.size + b.size) - 1)) {
        var _val: Complex = fa[i]!!
        res = run { val _tmp = res.toMutableList(); _tmp.add(round_to(_val.re, 8)); _tmp }
        i = i + 1
    }
    while ((res.size > 0) && (res[res.size - 1]!! == 0.0)) {
        res = _sliceList(res, 0, res.size - 1)
    }
    return res
}

fun main() {
    println(list_to_string(product))
}
