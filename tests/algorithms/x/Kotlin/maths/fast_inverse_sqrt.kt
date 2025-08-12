import java.math.BigInteger

fun lshift(x: Int, n: Int): Int {
return x * pow2(n)
}

fun pow2(n: Int): Int {
var v = 1
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

fun rshift(x: Int, n: Int): Int {
return x / pow2(n)
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun pow2_int(n: Int): Int {
    var result: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * 2
        i = i + 1
    }
    return result
}

fun pow2_float(n: Int): Double {
    var result: Double = 1.0
    if (n >= 0) {
        var i: Int = (0).toInt()
        while (i < n) {
            result = result * 2.0
            i = i + 1
        }
    } else {
        var i: Int = (0).toInt()
        var m: Int = (0 - n).toInt()
        while (i < m) {
            result = result / 2.0
            i = i + 1
        }
    }
    return result
}

fun log2_floor(x: Double): Int {
    var n: Double = x
    var e: Int = (0).toInt()
    while (n >= 2.0) {
        n = n / 2.0
        e = e + 1
    }
    while (n < 1.0) {
        n = n * 2.0
        e = e - 1
    }
    return e
}

fun float_to_bits(x: Double): Int {
    var num: Double = x
    var sign: Int = (0).toInt()
    if (num < 0.0) {
        sign = 1
        num = 0.0 - num
    }
    var exp: Int = (log2_floor(num)).toInt()
    var pow: Double = pow2_float(exp)
    var normalized: Double = num / pow
    var frac: Double = normalized - 1.0
    var mantissa: Int = (((frac * pow2_float(23)).toInt())).toInt()
    var exp_bits: Int = (exp + 127).toInt()
    return (lshift(sign, 31) + lshift(exp_bits, 23)) + mantissa
}

fun bits_to_float(bits: Int): Double {
    var sign_bit: Int = (Math.floorMod(rshift(bits, 31), 2)).toInt()
    var sign: Double = 1.0
    if (sign_bit == 1) {
        sign = 0.0 - 1.0
    }
    var exp_bits: Int = (Math.floorMod(rshift(bits, 23), 256)).toInt()
    var exp: Int = (exp_bits - 127).toInt()
    var mantissa_bits: Int = (Math.floorMod(bits, pow2_int(23))).toInt()
    var mantissa: Double = 1.0 + (((mantissa_bits.toDouble())) / pow2_float(23))
    return (sign * mantissa) * pow2_float(exp)
}

fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun sqrtApprox(x: Double): Double {
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

fun is_close(a: Double, b: Double, rel_tol: Double): Boolean {
    return absf(a - b) <= (rel_tol * absf(b))
}

fun fast_inverse_sqrt(number: Double): Double {
    if (number <= 0.0) {
        panic("Input must be a positive number.")
    }
    var i: Int = (float_to_bits(number)).toInt()
    var magic: Int = (1597463007).toInt()
    var y_bits: Int = (magic - rshift(i, 1)).toInt()
    var y: Double = bits_to_float(y_bits)
    y = y * (1.5 - (((0.5 * number) * y) * y))
    return y
}

fun test_fast_inverse_sqrt(): Unit {
    if (absf(fast_inverse_sqrt(10.0) - 0.3156857923527257) > 0.0001) {
        panic("fast_inverse_sqrt(10) failed")
    }
    if (absf(fast_inverse_sqrt(4.0) - 0.49915357479239103) > 0.0001) {
        panic("fast_inverse_sqrt(4) failed")
    }
    if (absf(fast_inverse_sqrt(4.1) - 0.4932849504615651) > 0.0001) {
        panic("fast_inverse_sqrt(4.1) failed")
    }
    var i: Int = (50).toInt()
    while (i < 60) {
        var y: Double = fast_inverse_sqrt((i.toDouble()))
        var actual: Double = 1.0 / sqrtApprox((i.toDouble()))
        if (!is_close(y, actual, 0.00132)) {
            panic("relative error too high")
        }
        i = i + 1
    }
}

fun user_main(): Unit {
    test_fast_inverse_sqrt()
    var i: Int = (5).toInt()
    while (i <= 100) {
        var diff: Double = (1.0 / sqrtApprox((i.toDouble()))) - fast_inverse_sqrt((i.toDouble()))
        println((i.toString() + ": ") + diff.toString())
        i = i + 5
    }
}

fun main() {
    user_main()
}
