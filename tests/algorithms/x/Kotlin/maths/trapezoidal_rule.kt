var a: Double = 0.0
var b: Double = 1.0
var steps: Double = 10.0
var boundary: MutableList<Double> = mutableListOf(a, b)
var y: Double = trapezoidal_rule(boundary, steps)
fun f(x: Double): Double {
    return x * x
}

fun make_points(a: Double, b: Double, h: Double): MutableList<Double> {
    var xs: MutableList<Double> = mutableListOf<Double>()
    var x: Double = a + h
    while (x <= (b - h)) {
        xs = run { val _tmp = xs.toMutableList(); _tmp.add(x); _tmp }
        x = x + h
    }
    return xs
}

fun trapezoidal_rule(boundary: MutableList<Double>, steps: Double): Double {
    var h: Double = (boundary[1]!! - boundary[0]!!) / steps
    var a: Double = boundary[0]!!
    var b: Double = boundary[1]!!
    var xs: MutableList<Double> = make_points(a, b, h)
    var y: Double = (h / 2.0) * f(a)
    var i: Int = (0).toInt()
    while (i < xs.size) {
        y = y + (h * f(xs[i]!!))
        i = i + 1
    }
    y = y + ((h / 2.0) * f(b))
    return y
}

fun main() {
    println("y = " + y.toString())
}
