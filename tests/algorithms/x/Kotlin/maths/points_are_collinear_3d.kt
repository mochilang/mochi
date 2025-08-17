fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Point3d(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)
data class Vector3d(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)
fun create_vector(p1: Point3d, p2: Point3d): Vector3d {
    var vx: Double = p2.x - p1.x
    var vy: Double = p2.y - p1.y
    var vz: Double = p2.z - p1.z
    return Vector3d(x = vx, y = vy, z = vz)
}

fun get_3d_vectors_cross(ab: Vector3d, ac: Vector3d): Vector3d {
    var cx: Double = (ab.y * ac.z) - (ab.z * ac.y)
    var cy: Double = (ab.z * ac.x) - (ab.x * ac.z)
    var cz: Double = (ab.x * ac.y) - (ab.y * ac.x)
    return Vector3d(x = cx, y = cy, z = cz)
}

fun pow10(exp: Int): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * 10.0
        i = i + 1
    }
    return result
}

fun round_float(x: Double, digits: Int): Double {
    var factor: Double = pow10(digits)
    var v: Double = x * factor
    if (v >= 0.0) {
        v = v + 0.5
    } else {
        v = v - 0.5
    }
    var t: Int = (v.toInt()).toInt()
    return (t.toDouble()) / factor
}

fun is_zero_vector(v: Vector3d, accuracy: Int): Boolean {
    return ((((round_float(v.x, accuracy) == 0.0) && (round_float(v.y, accuracy) == 0.0) as Boolean)) && (round_float(v.z, accuracy) == 0.0)) as Boolean
}

fun are_collinear(a: Point3d, b: Point3d, c: Point3d, accuracy: Int): Boolean {
    var ab: Vector3d = create_vector(a, b)
    var ac: Vector3d = create_vector(a, c)
    var cross: Vector3d = get_3d_vectors_cross(ab, ac)
    return is_zero_vector(cross, accuracy)
}

fun test_are_collinear(): Unit {
    var p1: Point3d = Point3d(x = 0.0, y = 0.0, z = 0.0)
    var p2: Point3d = Point3d(x = 1.0, y = 1.0, z = 1.0)
    var p3: Point3d = Point3d(x = 2.0, y = 2.0, z = 2.0)
    if (!are_collinear(p1, p2, p3, 10)) {
        panic("collinear test failed")
    }
    var q3: Point3d = Point3d(x = 1.0, y = 2.0, z = 3.0)
    if ((are_collinear(p1, p2, q3, 10)) as Boolean) {
        panic("non-collinear test failed")
    }
}

fun user_main(): Unit {
    test_are_collinear()
    var a: Point3d = Point3d(x = 4.802293498137402, y = 3.536233125455244, z = 0.0)
    var b: Point3d = Point3d(x = 0.0 - 2.186788107953106, y = 0.0 - 9.24561398001649, z = 7.141509524846482)
    var c: Point3d = Point3d(x = 1.530169574640268, y = 0.0 - 2.447927606600034, z = 3.343487096469054)
    println(are_collinear(a, b, c, 10).toString())
    var d: Point3d = Point3d(x = 2.399001826862445, y = 0.0 - 2.452009976680793, z = 4.464656666157666)
    var e: Point3d = Point3d(x = 0.0 - 3.682816335934376, y = 5.753788986533145, z = 9.490993909044244)
    var f: Point3d = Point3d(x = 1.962903518985307, y = 3.741415730125627, z = 7.0)
    println(are_collinear(d, e, f, 10).toString())
}

fun main() {
    user_main()
}
