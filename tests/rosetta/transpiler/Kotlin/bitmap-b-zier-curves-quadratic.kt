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

data class Pixel(var r: Int = 0, var g: Int = 0, var b: Int = 0)
var b2Seg: Int = 20
var b: MutableMap<String, Any?> = newBitmap(400, 300)
fun pixelFromRgb(rgb: Int): Pixel {
    var r: Int = ((Math.floorMod((rgb / 65536), 256)).toInt())
    var g: Int = ((Math.floorMod((rgb / 256), 256)).toInt())
    var b: Int = ((Math.floorMod(rgb, 256)).toInt())
    return Pixel(r = r, g = g, b = b)
}

fun newBitmap(cols: Int, rows: Int): MutableMap<String, Any?> {
    var d: MutableList<MutableList<Pixel>> = mutableListOf<MutableList<Pixel>>()
    var y: Int = 0
    while (y < rows) {
        var row: MutableList<Pixel> = mutableListOf<Pixel>()
        var x: Int = 0
        while (x < cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(Pixel(r = 0, g = 0, b = 0)); _tmp }
            x = x + 1
        }
        d = run { val _tmp = d.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return mutableMapOf<String, Any?>("cols" to (cols), "rows" to (rows), "data" to (d))
}

fun setPx(b: MutableMap<String, Any?>, x: Int, y: Int, p: Pixel): Unit {
    var cols: Int = (b)["cols"] as Int
    var rows: Int = (b)["rows"] as Int
    if ((((((x >= 0) && (x < cols) as Boolean)) && (y >= 0) as Boolean)) && (y < rows)) {
        (((b)["data"] as Any? as MutableList<Any?>)[y]!! as MutableList<Any?>)[x] = p
    }
}

fun fill(b: MutableMap<String, Any?>, p: Pixel): Unit {
    var cols: Int = (b)["cols"] as Int
    var rows: Int = (b)["rows"] as Int
    var y: Int = 0
    while (y < rows) {
        var x: Int = 0
        while (x < cols) {
            (((b)["data"] as Any? as MutableList<Any?>)[y]!! as MutableList<Any?>)[x] = p
            x = x + 1
        }
        y = y + 1
    }
}

fun fillRgb(b: MutableMap<String, Any?>, rgb: Int): Unit {
    fill(b, pixelFromRgb(rgb))
}

fun line(b: MutableMap<String, Any?>, x0: Int, y0: Int, x1: Int, y1: Int, p: Pixel): Unit {
    var y0: Int = y0
    var x0: Int = x0
    var dx: BigInteger = (x1 - x0).toBigInteger()
    if (dx.compareTo((0).toBigInteger()) < 0) {
        dx = (0).toBigInteger().subtract((dx))
    }
    var dy: BigInteger = (y1 - y0).toBigInteger()
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
        setPx(b, x0, y0, p)
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

fun bezier2(b: MutableMap<String, Any?>, x1: Int, y1: Int, x2: Int, y2: Int, x3: Int, y3: Int, p: Pixel): Unit {
    var px: MutableList<Int> = mutableListOf<Int>()
    var py: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i <= b2Seg) {
        px = run { val _tmp = px.toMutableList(); _tmp.add(0); _tmp }
        py = run { val _tmp = py.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var fx1: Double = (x1.toDouble())
    var fy1: Double = (y1.toDouble())
    var fx2: Double = (x2.toDouble())
    var fy2: Double = (y2.toDouble())
    var fx3: Double = (x3.toDouble())
    var fy3: Double = (y3.toDouble())
    i = 0
    while (i <= b2Seg) {
        var c: Double = ((i.toDouble())) / ((b2Seg.toDouble()))
        var a: Double = 1.0 - c
        var a2: Double = a * a
        var b2: Double = (2.0 * c) * a
        var c2: Double = c * c
        px[i] = ((((a2 * fx1) + (b2 * fx2)) + (c2 * fx3)).toInt())
        py[i] = ((((a2 * fy1) + (b2 * fy2)) + (c2 * fy3)).toInt())
        i = i + 1
    }
    var x0: Int = px[0]!!
    var y0: Int = py[0]!!
    i = 1
    while (i <= b2Seg) {
        var x: Int = px[i]!!
        var y: Int = py[i]!!
        line(b, x0, y0, x, y, p)
        x0 = x
        y0 = y
        i = i + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        fillRgb(b, 14614575)
        bezier2(b, 20, 150, 500, 0 - 100, 300, 280, pixelFromRgb(4165615))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
