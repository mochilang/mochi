fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun ceil_int(x: Double): Int {
    var n: Int = ((x.toInt())).toInt()
    if (((n.toDouble())) < x) {
        n = n + 1
    }
    return n
}

fun explicit_euler(ode_func: (Double, Double) -> Double, y0: Double, x0: Double, step_size: Double, x_end: Double): MutableList<Double> {
    var n: Int = (ceil_int((x_end - x0) / step_size)).toInt()
    var y: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i <= n) {
        y = run { val _tmp = y.toMutableList(); _tmp.add(0.0); _tmp }
        i = i + 1
    }
    _listSet(y, 0, y0)
    var x: Double = x0
    var k: Int = (0).toInt()
    while (k < n) {
        _listSet(y, k + 1, y[k]!! + (step_size * ode_func(x, y[k]!!)))
        x = x + step_size
        k = k + 1
    }
    return y
}

fun abs_float(a: Double): Double {
    if (a < 0.0) {
        return 0.0 - a
    }
    return a
}

fun test_explicit_euler(): Unit {
    var f: (Double, Double) -> Double = ({ x: Double, y: Double -> y } as (Double, Double) -> Double)
    var ys: MutableList<Double> = explicit_euler(f, 1.0, 0.0, 0.01, 5.0)
    var last: Double = ys[ys.size - 1]!!
    if (abs_float(last - 144.77277243257308) > 0.001) {
        panic("explicit_euler failed")
    }
}

fun user_main(): Unit {
    test_explicit_euler()
    var f: (Double, Double) -> Double = ({ x: Double, y: Double -> y } as (Double, Double) -> Double)
    var ys: MutableList<Double> = explicit_euler(f, 1.0, 0.0, 0.01, 5.0)
    println(ys[ys.size - 1]!!)
}

fun main() {
    user_main()
}
