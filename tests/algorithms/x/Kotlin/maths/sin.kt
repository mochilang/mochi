fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var PI: Double = 3.141592653589793
fun abs(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun floor(x: Double): Double {
    var i: Int = (x.toInt()).toInt()
    if ((i.toDouble()) > x) {
        i = i - 1
    }
    return i.toDouble()
}

fun pow(x: Double, n: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        result = result * x
        i = i + 1
    }
    return result
}

fun factorial(n: Int): Double {
    var result: Double = 1.0
    var i: Int = (2).toInt()
    while (i <= n) {
        result = result * (i.toDouble())
        i = i + 1
    }
    return result
}

fun radians(deg: Double): Double {
    return (deg * PI) / 180.0
}

fun taylor_sin(angle_in_degrees: Double, accuracy: Int, rounded_values_count: Int): Double {
    var k: Double = floor(angle_in_degrees / 360.0)
    var angle: Double = angle_in_degrees - (k * 360.0)
    var angle_in_radians: Double = radians(angle)
    var result: Double = angle_in_radians
    var a: Int = (3).toInt()
    var sign: Double = 0.0 - 1.0
    var i: Int = (0).toInt()
    while (i < accuracy) {
        result = result + ((sign * pow(angle_in_radians, a)) / factorial(a))
        sign = 0.0 - sign
        a = a + 2
        i = i + 1
    }
    return result
}

fun test_sin(): Unit {
    var eps: Double = 0.0000001
    if (abs(taylor_sin(0.0, 18, 10) - 0.0) > eps) {
        panic("sin(0) failed")
    }
    if (abs(taylor_sin(90.0, 18, 10) - 1.0) > eps) {
        panic("sin(90) failed")
    }
    if (abs(taylor_sin(180.0, 18, 10) - 0.0) > eps) {
        panic("sin(180) failed")
    }
    if (abs(taylor_sin(270.0, 18, 10) - (0.0 - 1.0)) > eps) {
        panic("sin(270) failed")
    }
}

fun user_main(): Unit {
    test_sin()
    var res: Double = taylor_sin(64.0, 18, 10)
    println(res)
}

fun main() {
    user_main()
}
