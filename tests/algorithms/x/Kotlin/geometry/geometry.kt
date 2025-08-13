fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class Angle(var degrees: Double = 0.0)
data class Side(var length: Double = 0.0, var angle: Angle = Angle(degrees = 0.0), var next: Int = 0)
data class Ellipse(var major: Double = 0.0, var minor: Double = 0.0)
data class Circle(var radius: Double = 0.0)
data class Polygon(var sides: MutableList<Side> = mutableListOf<Side>())
data class Rectangle(var short_side: Side = Side(length = 0.0, angle = Angle(degrees = 0.0), next = 0), var long_side: Side = Side(length = 0.0, angle = Angle(degrees = 0.0), next = 0), var poly: Polygon = Polygon(sides = mutableListOf<Side>()))
data class Square(var side: Side = Side(length = 0.0, angle = Angle(degrees = 0.0), next = 0), var rect: Rectangle = Rectangle(short_side = Side(length = 0.0, angle = Angle(degrees = 0.0), next = 0), long_side = Side(length = 0.0, angle = Angle(degrees = 0.0), next = 0), poly = Polygon(sides = mutableListOf<Side>())))
var PI: Double = 3.141592653589793
fun make_angle(deg: Double): Angle {
    if ((deg < 0.0) || (deg > 360.0)) {
        panic("degrees must be between 0 and 360")
    }
    return Angle(degrees = deg)
}

fun make_side(length: Double, angle: Angle): Side {
    if (length <= 0.0) {
        panic("length must be positive")
    }
    return Side(length = length, angle = angle, next = 0 - 1)
}

fun ellipse_area(e: Ellipse): Double {
    return (PI * e.major) * e.minor
}

fun ellipse_perimeter(e: Ellipse): Double {
    return PI * (e.major + e.minor)
}

fun circle_area(c: Circle): Double {
    var e: Ellipse = Ellipse(major = c.radius, minor = c.radius)
    var area: Double = ellipse_area(e)
    return area
}

fun circle_perimeter(c: Circle): Double {
    var e: Ellipse = Ellipse(major = c.radius, minor = c.radius)
    var per: Double = ellipse_perimeter(e)
    return per
}

fun circle_diameter(c: Circle): Double {
    return c.radius * 2.0
}

fun circle_max_parts(num_cuts: Double): Double {
    if (num_cuts < 0.0) {
        panic("num_cuts must be positive")
    }
    return ((num_cuts + 2.0) + (num_cuts * num_cuts)) * 0.5
}

fun make_polygon(): Polygon {
    var s: MutableList<Side> = mutableListOf<Side>()
    return Polygon(sides = s)
}

fun polygon_add_side(p: Polygon, s: Side): Unit {
    p.sides = run { val _tmp = (p.sides).toMutableList(); _tmp.add(s); _tmp }
}

fun polygon_get_side(p: Polygon, index: Int): Side {
    return (p.sides)[index]!!
}

fun polygon_set_side(p: Polygon, index: Int, s: Side): Unit {
    var tmp: MutableList<Side> = p.sides
    _listSet(tmp, index, s)
    p.sides = tmp
}

fun make_rectangle(short_len: Double, long_len: Double): Rectangle {
    if ((short_len <= 0.0) || (long_len <= 0.0)) {
        panic("length must be positive")
    }
    var short: Side = make_side(short_len, make_angle(90.0))
    var long: Side = make_side(long_len, make_angle(90.0))
    var p: Polygon = make_polygon()
    polygon_add_side(p, short)
    polygon_add_side(p, long)
    return Rectangle(short_side = short, long_side = long, poly = p)
}

fun rectangle_perimeter(r: Rectangle): Double {
    return (r.short_side.length + r.long_side.length) * 2.0
}

fun rectangle_area(r: Rectangle): Double {
    return r.short_side.length * r.long_side.length
}

fun make_square(side_len: Double): Square {
    var rect: Rectangle = make_rectangle(side_len, side_len)
    return Square(side = rect.short_side, rect = rect)
}

fun square_perimeter(s: Square): Double {
    var p: Double = rectangle_perimeter(s.rect)
    return p
}

fun square_area(s: Square): Double {
    var a: Double = rectangle_area(s.rect)
    return a
}

fun user_main(): Unit {
    var a: Angle = make_angle(90.0)
    println(a.degrees)
    var s: Side = make_side(5.0, a)
    println(s.length)
    var e: Ellipse = Ellipse(major = 5.0, minor = 10.0)
    println(ellipse_area(e))
    println(ellipse_perimeter(e))
    var c: Circle = Circle(radius = 5.0)
    println(circle_area(c))
    println(circle_perimeter(c))
    println(circle_diameter(c))
    println(circle_max_parts(7.0))
    var r: Rectangle = make_rectangle(5.0, 10.0)
    println(rectangle_perimeter(r))
    println(rectangle_area(r))
    var q: Square = make_square(5.0)
    println(square_perimeter(q))
    println(square_area(q))
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
