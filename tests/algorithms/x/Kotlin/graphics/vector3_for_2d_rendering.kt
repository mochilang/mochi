var PI: Double = 3.141592653589793
fun floor(x: Double): Double {
    var i: Int = ((x.toInt())).toInt()
    if (((i.toDouble())) > x) {
        i = i - 1
    }
    return (i.toDouble())
}

fun modf(x: Double, m: Double): Double {
    return x - (floor(x / m) * m)
}

fun sin_taylor(x: Double): Double {
    var term: Double = x
    var sum: Double = x
    var i: Int = (1).toInt()
    while (i < 10) {
        var k1: Double = 2.0 * ((i.toDouble()))
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
        var k1: Double = (2.0 * ((i.toDouble()))) - 1.0
        var k2: Double = 2.0 * ((i.toDouble()))
        term = (((0.0 - term) * x) * x) / (k1 * k2)
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun convert_to_2d(x: Double, y: Double, z: Double, scale: Double, distance: Double): MutableList<Double> {
    var projected_x: Double = ((x * distance) / (z + distance)) * scale
    var projected_y: Double = ((y * distance) / (z + distance)) * scale
    return mutableListOf(projected_x, projected_y)
}

fun rotate(x: Double, y: Double, z: Double, axis: String, angle: Double): MutableList<Double> {
    var angle: Double = ((modf(angle, 360.0) / 450.0) * 180.0) / PI
    angle = modf(angle, 2.0 * PI)
    if (angle > PI) {
        angle = angle - (2.0 * PI)
    }
    if (axis == "z") {
        var new_x: Double = (x * cos_taylor(angle)) - (y * sin_taylor(angle))
        var new_y: Double = (y * cos_taylor(angle)) + (x * sin_taylor(angle))
        var new_z: Double = z
        return mutableListOf(new_x, new_y, new_z)
    }
    if (axis == "x") {
        var new_y: Double = (y * cos_taylor(angle)) - (z * sin_taylor(angle))
        var new_z: Double = (z * cos_taylor(angle)) + (y * sin_taylor(angle))
        var new_x: Double = x
        return mutableListOf(new_x, new_y, new_z)
    }
    if (axis == "y") {
        var new_x: Double = (x * cos_taylor(angle)) - (z * sin_taylor(angle))
        var new_z: Double = (z * cos_taylor(angle)) + (x * sin_taylor(angle))
        var new_y: Double = y
        return mutableListOf(new_x, new_y, new_z)
    }
    println("not a valid axis, choose one of 'x', 'y', 'z'")
    return mutableListOf(0.0, 0.0, 0.0)
}

fun main() {
    println(convert_to_2d(1.0, 2.0, 3.0, 10.0, 10.0).toString())
    println(rotate(1.0, 2.0, 3.0, "y", 90.0).toString())
}
