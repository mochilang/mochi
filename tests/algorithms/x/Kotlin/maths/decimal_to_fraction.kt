import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Fraction(var numerator: Int = 0, var denominator: Int = 0)
fun pow10(n: Int): Int {
    var result: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * 10
        i = i + 1
    }
    return result
}

fun gcd(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    if (x < 0) {
        x = 0 - x
    }
    if (y < 0) {
        y = 0 - y
    }
    while (y != 0) {
        var r: Int = (Math.floorMod(x, y)).toInt()
        x = y
        y = r
    }
    return x
}

fun parse_decimal(s: String): Fraction {
    if (s.length == 0) {
        panic("invalid number")
    }
    var idx: Int = (0).toInt()
    var sign: Int = (1).toInt()
    var first: String = s.substring(0, 1)
    if (first == "-") {
        sign = 0 - 1
        idx = 1
    } else {
        if (first == "+") {
            idx = 1
        }
    }
    var int_part: String = ""
    while (idx < s.length) {
        var c: String = s.substring(idx, idx + 1)
        if ((c >= "0") && (c <= "9")) {
            int_part = int_part + c
            idx = idx + 1
        } else {
            break
        }
    }
    var frac_part: String = ""
    if ((idx < s.length) && (s.substring(idx, idx + 1) == ".")) {
        idx = idx + 1
        while (idx < s.length) {
            var c: String = s.substring(idx, idx + 1)
            if ((c >= "0") && (c <= "9")) {
                frac_part = frac_part + c
                idx = idx + 1
            } else {
                break
            }
        }
    }
    var exp: Int = (0).toInt()
    if ((idx < s.length) && (((s.substring(idx, idx + 1) == "e") || (s.substring(idx, idx + 1) == "E") as Boolean))) {
        idx = idx + 1
        var exp_sign: Int = (1).toInt()
        if ((idx < s.length) && (s.substring(idx, idx + 1) == "-")) {
            exp_sign = 0 - 1
            idx = idx + 1
        } else {
            if ((idx < s.length) && (s.substring(idx, idx + 1) == "+")) {
                idx = idx + 1
            }
        }
        var exp_str: String = ""
        while (idx < s.length) {
            var c: String = s.substring(idx, idx + 1)
            if ((c >= "0") && (c <= "9")) {
                exp_str = exp_str + c
                idx = idx + 1
            } else {
                panic("invalid number")
            }
        }
        if (exp_str.length == 0) {
            panic("invalid number")
        }
        exp = exp_sign * ((exp_str.toBigInteger().toInt()))
    }
    if (idx != s.length) {
        panic("invalid number")
    }
    if (int_part.length == 0) {
        int_part = "0"
    }
    var num_str: String = int_part + frac_part
    var numerator: Int = ((num_str.toBigInteger().toInt())).toInt()
    if (sign == (0 - 1)) {
        numerator = 0 - numerator
    }
    var denominator: Int = (pow10(frac_part.length)).toInt()
    if (exp > 0) {
        numerator = numerator * pow10(exp)
    } else {
        if (exp < 0) {
            denominator = denominator * pow10(0 - exp)
        }
    }
    return Fraction(numerator = numerator, denominator = denominator)
}

fun reduce(fr: Fraction): Fraction {
    var g: Int = (gcd(fr.numerator, fr.denominator)).toInt()
    return Fraction(numerator = fr.numerator / g, denominator = fr.denominator / g)
}

fun decimal_to_fraction_str(s: String): Fraction {
    return reduce(parse_decimal(s))
}

fun decimal_to_fraction(x: Double): Fraction {
    return decimal_to_fraction_str(x.toString())
}

fun assert_fraction(name: String, fr: Fraction, num: Int, den: Int): Unit {
    if ((fr.numerator != num) || (fr.denominator != den)) {
        panic(name)
    }
}

fun test_decimal_to_fraction(): Unit {
    assert_fraction("case1", decimal_to_fraction(2.0), 2, 1)
    assert_fraction("case2", decimal_to_fraction(89.0), 89, 1)
    assert_fraction("case3", decimal_to_fraction_str("67"), 67, 1)
    assert_fraction("case4", decimal_to_fraction_str("45.0"), 45, 1)
    assert_fraction("case5", decimal_to_fraction(1.5), 3, 2)
    assert_fraction("case6", decimal_to_fraction_str("6.25"), 25, 4)
    assert_fraction("case7", decimal_to_fraction(0.0), 0, 1)
    assert_fraction("case8", decimal_to_fraction(0.0 - 2.5), 0 - 5, 2)
    assert_fraction("case9", decimal_to_fraction(0.125), 1, 8)
    assert_fraction("case10", decimal_to_fraction(1000000.25), 4000001, 4)
    assert_fraction("case11", decimal_to_fraction(1.3333), 13333, 10000)
    assert_fraction("case12", decimal_to_fraction_str("1.23e2"), 123, 1)
    assert_fraction("case13", decimal_to_fraction_str("0.500"), 1, 2)
}

fun user_main(): Unit {
    test_decimal_to_fraction()
    var fr: Fraction = decimal_to_fraction(1.5)
    println((fr.numerator.toString() + "/") + fr.denominator.toString())
}

fun main() {
    user_main()
}
