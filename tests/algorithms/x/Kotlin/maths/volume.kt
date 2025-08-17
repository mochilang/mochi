fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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
var SQRT5: Double = 2.23606797749979
fun minf(a: Double, b: Double): Double {
    if (a < b) {
        return a
    }
    return b
}

fun maxf(a: Double, b: Double): Double {
    if (a > b) {
        return a
    }
    return b
}

fun vol_cube(side_length: Double): Double {
    if (side_length < 0.0) {
        panic("vol_cube() only accepts non-negative values")
    }
    return (side_length * side_length) * side_length
}

fun vol_spherical_cap(height: Double, radius: Double): Double {
    if ((height < 0.0) || (radius < 0.0)) {
        panic("vol_spherical_cap() only accepts non-negative values")
    }
    return ((((1.0 / 3.0) * PI) * height) * height) * ((3.0 * radius) - height)
}

fun vol_sphere(radius: Double): Double {
    if (radius < 0.0) {
        panic("vol_sphere() only accepts non-negative values")
    }
    return ((((4.0 / 3.0) * PI) * radius) * radius) * radius
}

fun vol_spheres_intersect(radius_1: Double, radius_2: Double, centers_distance: Double): Double {
    if ((((radius_1 < 0.0) || (radius_2 < 0.0) as Boolean)) || (centers_distance < 0.0)) {
        panic("vol_spheres_intersect() only accepts non-negative values")
    }
    if (centers_distance == 0.0) {
        return vol_sphere(minf(radius_1, radius_2))
    }
    var h1: Double = (((radius_1 - radius_2) + centers_distance) * ((radius_1 + radius_2) - centers_distance)) / (2.0 * centers_distance)
    var h2: Double = (((radius_2 - radius_1) + centers_distance) * ((radius_2 + radius_1) - centers_distance)) / (2.0 * centers_distance)
    return vol_spherical_cap(h1, radius_2) + vol_spherical_cap(h2, radius_1)
}

fun vol_spheres_union(radius_1: Double, radius_2: Double, centers_distance: Double): Double {
    if ((((radius_1 <= 0.0) || (radius_2 <= 0.0) as Boolean)) || (centers_distance < 0.0)) {
        panic("vol_spheres_union() only accepts non-negative values, non-zero radius")
    }
    if (centers_distance == 0.0) {
        return vol_sphere(maxf(radius_1, radius_2))
    }
    return (vol_sphere(radius_1) + vol_sphere(radius_2)) - vol_spheres_intersect(radius_1, radius_2, centers_distance)
}

fun vol_cuboid(width: Double, height: Double, length: Double): Double {
    if ((((width < 0.0) || (height < 0.0) as Boolean)) || (length < 0.0)) {
        panic("vol_cuboid() only accepts non-negative values")
    }
    return (width * height) * length
}

fun vol_cone(area_of_base: Double, height: Double): Double {
    if ((height < 0.0) || (area_of_base < 0.0)) {
        panic("vol_cone() only accepts non-negative values")
    }
    return (area_of_base * height) / 3.0
}

fun vol_right_circ_cone(radius: Double, height: Double): Double {
    if ((height < 0.0) || (radius < 0.0)) {
        panic("vol_right_circ_cone() only accepts non-negative values")
    }
    return (((PI * radius) * radius) * height) / 3.0
}

fun vol_prism(area_of_base: Double, height: Double): Double {
    if ((height < 0.0) || (area_of_base < 0.0)) {
        panic("vol_prism() only accepts non-negative values")
    }
    return area_of_base * height
}

fun vol_pyramid(area_of_base: Double, height: Double): Double {
    if ((height < 0.0) || (area_of_base < 0.0)) {
        panic("vol_pyramid() only accepts non-negative values")
    }
    return (area_of_base * height) / 3.0
}

fun vol_hemisphere(radius: Double): Double {
    if (radius < 0.0) {
        panic("vol_hemisphere() only accepts non-negative values")
    }
    return ((((radius * radius) * radius) * PI) * 2.0) / 3.0
}

fun vol_circular_cylinder(radius: Double, height: Double): Double {
    if ((height < 0.0) || (radius < 0.0)) {
        panic("vol_circular_cylinder() only accepts non-negative values")
    }
    return ((radius * radius) * height) * PI
}

fun vol_hollow_circular_cylinder(inner_radius: Double, outer_radius: Double, height: Double): Double {
    if ((((inner_radius < 0.0) || (outer_radius < 0.0) as Boolean)) || (height < 0.0)) {
        panic("vol_hollow_circular_cylinder() only accepts non-negative values")
    }
    if (outer_radius <= inner_radius) {
        panic("outer_radius must be greater than inner_radius")
    }
    return (PI * ((outer_radius * outer_radius) - (inner_radius * inner_radius))) * height
}

fun vol_conical_frustum(height: Double, radius_1: Double, radius_2: Double): Double {
    if ((((radius_1 < 0.0) || (radius_2 < 0.0) as Boolean)) || (height < 0.0)) {
        panic("vol_conical_frustum() only accepts non-negative values")
    }
    return (((1.0 / 3.0) * PI) * height) * (((radius_1 * radius_1) + (radius_2 * radius_2)) + (radius_1 * radius_2))
}

fun vol_torus(torus_radius: Double, tube_radius: Double): Double {
    if ((torus_radius < 0.0) || (tube_radius < 0.0)) {
        panic("vol_torus() only accepts non-negative values")
    }
    return ((((2.0 * PI) * PI) * torus_radius) * tube_radius) * tube_radius
}

fun vol_icosahedron(tri_side: Double): Double {
    if (tri_side < 0.0) {
        panic("vol_icosahedron() only accepts non-negative values")
    }
    return ((((tri_side * tri_side) * tri_side) * (3.0 + SQRT5)) * 5.0) / 12.0
}

fun user_main(): Unit {
    println("Volumes:")
    println("Cube: " + _numToStr(vol_cube(2.0)))
    println("Cuboid: " + _numToStr(vol_cuboid(2.0, 2.0, 2.0)))
    println("Cone: " + _numToStr(vol_cone(2.0, 2.0)))
    println("Right Circular Cone: " + _numToStr(vol_right_circ_cone(2.0, 2.0)))
    println("Prism: " + _numToStr(vol_prism(2.0, 2.0)))
    println("Pyramid: " + _numToStr(vol_pyramid(2.0, 2.0)))
    println("Sphere: " + _numToStr(vol_sphere(2.0)))
    println("Hemisphere: " + _numToStr(vol_hemisphere(2.0)))
    println("Circular Cylinder: " + _numToStr(vol_circular_cylinder(2.0, 2.0)))
    println("Torus: " + _numToStr(vol_torus(2.0, 2.0)))
    println("Conical Frustum: " + _numToStr(vol_conical_frustum(2.0, 2.0, 4.0)))
    println("Spherical cap: " + _numToStr(vol_spherical_cap(1.0, 2.0)))
    println("Spheres intersection: " + _numToStr(vol_spheres_intersect(2.0, 2.0, 1.0)))
    println("Spheres union: " + _numToStr(vol_spheres_union(2.0, 2.0, 1.0)))
    println("Hollow Circular Cylinder: " + _numToStr(vol_hollow_circular_cylinder(1.0, 2.0, 3.0)))
    println("Icosahedron: " + _numToStr(vol_icosahedron(2.5)))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
