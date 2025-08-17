fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Point(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)
fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun sqrt_approx(x: Double): Double {
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

fun distance(a: Point, b: Point): Double {
    var dx: Double = b.x - a.x
    var dy: Double = b.y - a.y
    var dz: Double = b.z - a.z
    return sqrt_approx(absf(((dx * dx) + (dy * dy)) + (dz * dz)))
}

fun point_to_string(p: Point): String {
    return ((((("Point(" + _numToStr(p.x)) + ", ") + _numToStr(p.y)) + ", ") + _numToStr(p.z)) + ")"
}

fun test_distance(): Unit {
    var p1: Point = Point(x = 2.0, y = 0.0 - 1.0, z = 7.0)
    var p2: Point = Point(x = 1.0, y = 0.0 - 3.0, z = 5.0)
    var d: Double = distance(p1, p2)
    if (absf(d - 3.0) > 0.0001) {
        panic("distance test failed")
    }
    println((((("Distance from " + point_to_string(p1)) + " to ") + point_to_string(p2)) + " is ") + _numToStr(d))
}

fun user_main(): Unit {
    test_distance()
}

fun main() {
    user_main()
}
