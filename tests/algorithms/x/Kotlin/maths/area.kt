var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

var PI: Double = 3.141592653589793
var TWO_PI: Double = 6.283185307179586
fun _mod(x: Double, m: Double): Double {
    return x - ((((((x / m).toInt())).toDouble())) * m)
}

fun sin_approx(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y3: Double = y2 * y
    var y5: Double = y3 * y2
    var y7: Double = y5 * y2
    return ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
}

fun cos_approx(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y4: Double = y2 * y2
    var y6: Double = y4 * y2
    return ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
}

fun tan_approx(x: Double): Double {
    return sin_approx(x) / cos_approx(x)
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

fun surface_area_cube(side_length: Double): Double {
    if (side_length < 0.0) {
        println("ValueError: surface_area_cube() only accepts non-negative values")
        return 0.0
    }
    return (6.0 * side_length) * side_length
}

fun surface_area_cuboid(length: Double, breadth: Double, height: Double): Double {
    if ((((length < 0.0) || (breadth < 0.0) as Boolean)) || (height < 0.0)) {
        println("ValueError: surface_area_cuboid() only accepts non-negative values")
        return 0.0
    }
    return 2.0 * (((length * breadth) + (breadth * height)) + (length * height))
}

fun surface_area_sphere(radius: Double): Double {
    if (radius < 0.0) {
        println("ValueError: surface_area_sphere() only accepts non-negative values")
        return 0.0
    }
    return ((4.0 * PI) * radius) * radius
}

fun surface_area_hemisphere(radius: Double): Double {
    if (radius < 0.0) {
        println("ValueError: surface_area_hemisphere() only accepts non-negative values")
        return 0.0
    }
    return ((3.0 * PI) * radius) * radius
}

fun surface_area_cone(radius: Double, height: Double): Double {
    if ((radius < 0.0) || (height < 0.0)) {
        println("ValueError: surface_area_cone() only accepts non-negative values")
        return 0.0
    }
    var slant: Double = sqrt_approx((height * height) + (radius * radius))
    return (PI * radius) * (radius + slant)
}

fun surface_area_conical_frustum(radius1: Double, radius2: Double, height: Double): Double {
    if ((((radius1 < 0.0) || (radius2 < 0.0) as Boolean)) || (height < 0.0)) {
        println("ValueError: surface_area_conical_frustum() only accepts non-negative values")
        return 0.0
    }
    var slant: Double = sqrt_approx((height * height) + ((radius1 - radius2) * (radius1 - radius2)))
    return PI * (((slant * (radius1 + radius2)) + (radius1 * radius1)) + (radius2 * radius2))
}

fun surface_area_cylinder(radius: Double, height: Double): Double {
    if ((radius < 0.0) || (height < 0.0)) {
        println("ValueError: surface_area_cylinder() only accepts non-negative values")
        return 0.0
    }
    return ((2.0 * PI) * radius) * (height + radius)
}

fun surface_area_torus(torus_radius: Double, tube_radius: Double): Double {
    if ((torus_radius < 0.0) || (tube_radius < 0.0)) {
        println("ValueError: surface_area_torus() only accepts non-negative values")
        return 0.0
    }
    if (torus_radius < tube_radius) {
        println("ValueError: surface_area_torus() does not support spindle or self intersecting tori")
        return 0.0
    }
    return (((4.0 * PI) * PI) * torus_radius) * tube_radius
}

fun area_rectangle(length: Double, width: Double): Double {
    if ((length < 0.0) || (width < 0.0)) {
        println("ValueError: area_rectangle() only accepts non-negative values")
        return 0.0
    }
    return length * width
}

fun area_square(side_length: Double): Double {
    if (side_length < 0.0) {
        println("ValueError: area_square() only accepts non-negative values")
        return 0.0
    }
    return side_length * side_length
}

fun area_triangle(base: Double, height: Double): Double {
    if ((base < 0.0) || (height < 0.0)) {
        println("ValueError: area_triangle() only accepts non-negative values")
        return 0.0
    }
    return (base * height) / 2.0
}

fun area_triangle_three_sides(side1: Double, side2: Double, side3: Double): Double {
    if ((((side1 < 0.0) || (side2 < 0.0) as Boolean)) || (side3 < 0.0)) {
        println("ValueError: area_triangle_three_sides() only accepts non-negative values")
        return 0.0
    }
    if (((((side1 + side2) < side3) || ((side1 + side3) < side2) as Boolean)) || ((side2 + side3) < side1)) {
        println("ValueError: Given three sides do not form a triangle")
        return 0.0
    }
    var s: Double = ((side1 + side2) + side3) / 2.0
    var prod: Double = ((s * (s - side1)) * (s - side2)) * (s - side3)
    var res: Double = sqrt_approx(prod)
    return res
}

fun area_parallelogram(base: Double, height: Double): Double {
    if ((base < 0.0) || (height < 0.0)) {
        println("ValueError: area_parallelogram() only accepts non-negative values")
        return 0.0
    }
    return base * height
}

fun area_trapezium(base1: Double, base2: Double, height: Double): Double {
    if ((((base1 < 0.0) || (base2 < 0.0) as Boolean)) || (height < 0.0)) {
        println("ValueError: area_trapezium() only accepts non-negative values")
        return 0.0
    }
    return (0.5 * (base1 + base2)) * height
}

fun area_circle(radius: Double): Double {
    if (radius < 0.0) {
        println("ValueError: area_circle() only accepts non-negative values")
        return 0.0
    }
    return (PI * radius) * radius
}

fun area_ellipse(radius_x: Double, radius_y: Double): Double {
    if ((radius_x < 0.0) || (radius_y < 0.0)) {
        println("ValueError: area_ellipse() only accepts non-negative values")
        return 0.0
    }
    return (PI * radius_x) * radius_y
}

fun area_rhombus(diagonal1: Double, diagonal2: Double): Double {
    if ((diagonal1 < 0.0) || (diagonal2 < 0.0)) {
        println("ValueError: area_rhombus() only accepts non-negative values")
        return 0.0
    }
    return (0.5 * diagonal1) * diagonal2
}

fun area_reg_polygon(sides: Int, length: Double): Double {
    if (sides < 3) {
        println("ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides")
        return 0.0
    }
    if (length < 0.0) {
        println("ValueError: area_reg_polygon() only accepts non-negative values as length of a side")
        return 0.0
    }
    var n: Double = (sides.toDouble())
    return ((n * length) * length) / (4.0 * tan_approx(PI / n))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("[DEMO] Areas of various geometric shapes:")
        println("Rectangle: " + area_rectangle(10.0, 20.0).toString())
        println("Square: " + area_square(10.0).toString())
        println("Triangle: " + area_triangle(10.0, 10.0).toString())
        var TRI_THREE_SIDES: Double = area_triangle_three_sides(5.0, 12.0, 13.0)
        println("Triangle Three Sides: " + TRI_THREE_SIDES.toString())
        println("Parallelogram: " + area_parallelogram(10.0, 20.0).toString())
        println("Rhombus: " + area_rhombus(10.0, 20.0).toString())
        println("Trapezium: " + area_trapezium(10.0, 20.0, 30.0).toString())
        println("Circle: " + area_circle(20.0).toString())
        println("Ellipse: " + area_ellipse(10.0, 20.0).toString())
        println("")
        println("Surface Areas of various geometric shapes:")
        println("Cube: " + surface_area_cube(20.0).toString())
        println("Cuboid: " + surface_area_cuboid(10.0, 20.0, 30.0).toString())
        println("Sphere: " + surface_area_sphere(20.0).toString())
        println("Hemisphere: " + surface_area_hemisphere(20.0).toString())
        println("Cone: " + surface_area_cone(10.0, 20.0).toString())
        println("Conical Frustum: " + surface_area_conical_frustum(10.0, 20.0, 30.0).toString())
        println("Cylinder: " + surface_area_cylinder(10.0, 20.0).toString())
        println("Torus: " + surface_area_torus(20.0, 10.0).toString())
        println("Equilateral Triangle: " + area_reg_polygon(3, 10.0).toString())
        println("Square: " + area_reg_polygon(4, 10.0).toString())
        println("Regular Pentagon: " + area_reg_polygon(5, 10.0).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
