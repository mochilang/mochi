import java.math.BigInteger

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

data class Point3(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)
data class Point2(var x: Int = 0, var y: Int = 0)
var PI: Double = 3.141592653589793
var TWO_PI: Double = 6.283185307179586
var nodes: MutableList<Point3> = mutableListOf(Point3(x = 0.0 - 1.0, y = 0.0 - 1.0, z = 0.0 - 1.0), Point3(x = 0.0 - 1.0, y = 0.0 - 1.0, z = 1.0), Point3(x = 0.0 - 1.0, y = 1.0, z = 0.0 - 1.0), Point3(x = 0.0 - 1.0, y = 1.0, z = 1.0), Point3(x = 1.0, y = 0.0 - 1.0, z = 0.0 - 1.0), Point3(x = 1.0, y = 0.0 - 1.0, z = 1.0), Point3(x = 1.0, y = 1.0, z = 0.0 - 1.0), Point3(x = 1.0, y = 1.0, z = 1.0))
var edges: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1), mutableListOf(1, 3), mutableListOf(3, 2), mutableListOf(2, 0), mutableListOf(4, 5), mutableListOf(5, 7), mutableListOf(7, 6), mutableListOf(6, 4), mutableListOf(0, 4), mutableListOf(1, 5), mutableListOf(2, 6), mutableListOf(3, 7))
var width: Int = 40
var height: Int = 20
var distance: Double = 3.0
var scale: Double = 8.0
fun _mod(x: Double, m: Double): Double {
    return x - ((((((x / m).toInt())).toDouble())) * m)
}

fun _sin(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y3: Double = y2 * y
    var y5: Double = y3 * y2
    var y7: Double = y5 * y2
    return ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
}

fun _cos(x: Double): Double {
    var y: Double = _mod(x + PI, TWO_PI) - PI
    var y2: Double = y * y
    var y4: Double = y2 * y2
    var y6: Double = y4 * y2
    return ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
}

fun rotate(p: Point3, ax: Double, ay: Double): Point3 {
    var sinx: Double = _sin(ax)
    var cosx: Double = _cos(ax)
    var siny: Double = _sin(ay)
    var cosy: Double = _cos(ay)
    var x1: Double = p.x
    var y1: Double = (p.y * cosx) - (p.z * sinx)
    var z1: Double = (p.y * sinx) + (p.z * cosx)
    var x2: Double = (x1 * cosy) + (z1 * siny)
    var z2: Double = ((0.0 - x1) * siny) + (z1 * cosy)
    return Point3(x = x2, y = y1, z = z2)
}

fun project(p: Point3): Point2 {
    var factor: Double = scale / (p.z + distance)
    var x: BigInteger = ((((p.x * factor).toInt())) + (width / 2)).toBigInteger()
    var y: BigInteger = (((((0.0 - p.y) * factor).toInt())) + (height / 2)).toBigInteger()
    return Point2(x = (x.toInt()), y = (y.toInt()))
}

fun clearGrid(): MutableList<MutableList<String>> {
    var g: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var y: Int = 0
    while (y < height) {
        var row: MutableList<String> = mutableListOf<String>()
        var x: Int = 0
        while (x < width) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(" "); _tmp }
            x = x + 1
        }
        g = run { val _tmp = g.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return g
}

fun drawPoint(g: MutableList<MutableList<String>>, x: Int, y: Int, ch: String): Unit {
    if ((((((x >= 0) && (x < width) as Boolean)) && (y >= 0) as Boolean)) && (y < height)) {
        var row: MutableList<String> = g[y]!!
        row[x] = ch
        g[y] = row
    }
}

fun bresenham(x0: Int, y0: Int, x1: Int, y1: Int, g: MutableList<MutableList<String>>, ch: String): Unit {
    var y0: Int = y0
    var x0: Int = x0
    var dx: BigInteger = ((x1 - x0).toBigInteger())
    if (dx.compareTo((0).toBigInteger()) < 0) {
        dx = (0).toBigInteger().subtract((dx))
    }
    var dy: BigInteger = ((y1 - y0).toBigInteger())
    if (dy.compareTo((0).toBigInteger()) < 0) {
        dy = (0).toBigInteger().subtract((dy))
    }
    var sx: Int = 0 - 1
    if (x0 < x1) {
        sx = 1
    }
    var sy: Int = 0 - 1
    if (y0 < y1) {
        sy = 1
    }
    var err: BigInteger = dx.subtract((dy))
    while (true) {
        drawPoint(g, x0, y0, ch)
        if ((x0 == x1) && (y0 == y1)) {
            break
        }
        var e2: BigInteger = (2).toBigInteger().multiply((err))
        if (e2.compareTo(((0).toBigInteger().subtract((dy)))) > 0) {
            err = err.subtract((dy))
            x0 = x0 + sx
        }
        if (e2.compareTo((dx)) < 0) {
            err = err.add((dx))
            y0 = y0 + sy
        }
    }
}

fun render(g: MutableList<MutableList<String>>): String {
    var out: String = ""
    var y: Int = 0
    while (y < height) {
        var line: String = ""
        var x: Int = 0
        while (x < width) {
            line = line + (((g[y]!!) as MutableList<String>))[x]!!
            x = x + 1
        }
        out = (out + line) + "\n"
        y = y + 1
    }
    return out
}

fun user_main(): Unit {
    var f: Int = 0
    while (f < 10) {
        var grid: MutableList<MutableList<String>> = clearGrid()
        var rot: MutableList<Point2> = mutableListOf<Point2>()
        var i: Int = 0
        var ay: Double = (PI / 4.0) + ((((f.toDouble())) * PI) / 10.0)
        while (i < nodes.size) {
            var p: Point3 = rotate(nodes[i]!!, PI / 4.0, ay)
            var pp: Point2 = project(p)
            rot = run { val _tmp = rot.toMutableList(); _tmp.add(pp); _tmp }
            i = i + 1
        }
        var e: Int = 0
        while (e < edges.size) {
            var a: Int = (((edges[e]!!) as MutableList<Int>))[0]!!
            var b: Int = (((edges[e]!!) as MutableList<Int>))[1]!!
            var p1: Point2 = rot[a]!!
            var p2: Point2 = rot[b]!!
            bresenham(p1.x, p1.y, p2.x, p2.y, grid, "#")
            e = e + 1
        }
        println(render(grid))
        f = f + 1
    }
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
