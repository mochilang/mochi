fun ceil_float(x: Double): Int {
    var i: Int = ((x.toInt())).toInt()
    if (x > ((i.toDouble()))) {
        return i + 1
    }
    return i
}

fun exp_approx(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun euler_modified(ode_func: (Double, Double) -> Double, y0: Double, x0: Double, step: Double, x_end: Double): MutableList<Double> {
    var n: Int = (ceil_float((x_end - x0) / step)).toInt()
    var y: MutableList<Double> = mutableListOf(y0)
    var x: Double = x0
    var k: Int = (0).toInt()
    while (k < n) {
        var y_predict: Double = y[k]!! + (step * ode_func(x, y[k]!!))
        var slope1: Double = ((ode_func(x, y[k]!!)).toDouble())
        var slope2: Double = ((ode_func(x + step, y_predict)).toDouble())
        var y_next: Double = y[k]!! + ((step / 2.0) * (slope1 + slope2))
        y = run { val _tmp = y.toMutableList(); _tmp.add(y_next); _tmp }
        x = x + step
        k = k + 1
    }
    return y
}

fun f1(x: Double, y: Double): Double {
    return (((0.0 - 2.0) * x) * y) * y
}

fun f2(x: Double, y: Double): Double {
    return ((0.0 - 2.0) * y) + (((x * x) * x) * exp_approx((0.0 - 2.0) * x))
}

fun user_main(): Unit {
    var y1: MutableList<Double> = euler_modified(::f1, 1.0, 0.0, 0.2, 1.0)
    println(y1[y1.size - 1]!!)
    var y2: MutableList<Double> = euler_modified(::f2, 1.0, 0.0, 0.1, 0.3)
    println(y2[y2.size - 1]!!)
}

fun main() {
    user_main()
}
