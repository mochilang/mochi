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

var PI: Double = 3.141592653589793
var width: Int = 80
var height: Int = 40
var depth: Int = 6
var angle: Double = 12.0
var length: Double = 12.0
var frac: Double = 0.8
fun _mod(x: Double, m: Double): Double {
    return x - (((x / m).toInt()).toDouble() * m)
}

fun _sin(x: Double): Double {
    var y: Double = _mod(x + PI, 2.0 * PI) - PI
    var y2: Double = y * y
    var y3: Double = y2 * y
    var y5: Double = y3 * y2
    var y7: Double = y5 * y2
    return ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
}

fun _cos(x: Double): Double {
    var y: Double = _mod(x + PI, 2.0 * PI) - PI
    var y2: Double = y * y
    var y4: Double = y2 * y2
    var y6: Double = y4 * y2
    return ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
}

fun clearGrid(): MutableList<MutableList<String>> {
    var g: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var y: Int = 0
    while (y < height) {
        var row: MutableList<String> = mutableListOf<String>()
        var x: Int = 0
        while (x < width) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(" "); _tmp } as MutableList<String>
            x = x + 1
        }
        g = run { val _tmp = g.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<String>>
        y = y + 1
    }
    return g
}

fun drawPoint(g: MutableList<MutableList<String>>, x: Int, y: Int): Unit {
    if ((((((x >= 0) && (x < width) as Boolean)) && (y >= 0) as Boolean)) && (y < height)) {
        var row: MutableList<String> = g[y]!!
        (row[x]) = "#"
        (g[y]) = row
    }
}

fun bresenham(x0: Int, y0: Int, x1: Int, y1: Int, g: MutableList<MutableList<String>>): Unit {
    var y0: Int = y0
    var x0: Int = x0
    var dx: BigInteger = (x1 - x0).toBigInteger()
    if (dx.compareTo(0.toBigInteger()) < 0) {
        dx = (0).toBigInteger().subtract(dx)
    }
    var dy: BigInteger = (y1 - y0).toBigInteger()
    if (dy.compareTo(0.toBigInteger()) < 0) {
        dy = (0).toBigInteger().subtract(dy)
    }
    var sx: Int = 0 - 1
    if (x0 < x1) {
        sx = 1
    }
    var sy: Int = 0 - 1
    if (y0 < y1) {
        sy = 1
    }
    var err: BigInteger = dx.subtract(dy)
    while (true) {
        drawPoint(g, x0, y0)
        if ((x0 == x1) && (y0 == y1)) {
            break
        }
        var e2: BigInteger = (2).toBigInteger().multiply(err)
        if (e2.compareTo(((0).toBigInteger().subtract(dy))) > 0) {
            err = err.subtract(dy)
            x0 = x0 + sx
        }
        if (e2.compareTo(dx) < 0) {
            err = err.add(dx)
            y0 = y0 + sy
        }
    }
}

fun ftree(g: MutableList<MutableList<String>>, x: Double, y: Double, dist: Double, dir: Double, d: Int): Unit {
    var rad: Double = (dir * PI) / 180.0
    var x2: Double = x + (dist * _sin(rad))
    var y2: Double = y - (dist * _cos(rad))
    bresenham(x.toInt(), y.toInt(), x2.toInt(), y2.toInt(), g)
    if (d > 0) {
        ftree(g, x2, y2, dist * frac, dir - angle, d - 1)
        ftree(g, x2, y2, dist * frac, dir + angle, d - 1)
    }
}

fun render(g: MutableList<MutableList<String>>): String {
    var out: String = ""
    var y: Int = 0
    while (y < height) {
        var line: String = ""
        var x: Int = 0
        while (x < width) {
            line = line + ((g[y]!!) as MutableList<String>)[x]!!
            x = x + 1
        }
        out = out + line
        if (y < (height - 1)) {
            out = out + "\n"
        }
        y = y + 1
    }
    return out
}

fun user_main(): Unit {
    var grid: MutableList<MutableList<String>> = clearGrid()
    ftree(grid, (width / 2).toDouble(), (height - 1).toDouble(), length, 0.0, depth)
    println(render(grid))
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
