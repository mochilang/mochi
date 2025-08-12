fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
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

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return floor((x * m) + 0.5) / m
}

fun decimal_isolate(number: Double, digit_amount: Int): Double {
    var whole: Int = ((number.toInt())).toInt()
    var frac: Double = number - ((whole.toDouble()))
    if (digit_amount > 0) {
        return round(frac, digit_amount)
    }
    return frac
}

fun user_main(): Unit {
    println(decimal_isolate(1.53, 0).toString())
    println(decimal_isolate(35.345, 1).toString())
    println(decimal_isolate(35.345, 2).toString())
    println(decimal_isolate(35.345, 3).toString())
    println(decimal_isolate(0.0 - 14.789, 3).toString())
    println(decimal_isolate(0.0, 2).toString())
    println(decimal_isolate(0.0 - 14.123, 1).toString())
    println(decimal_isolate(0.0 - 14.123, 2).toString())
    println(decimal_isolate(0.0 - 14.123, 3).toString())
}

fun main() {
    user_main()
}
