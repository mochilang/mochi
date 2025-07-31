import java.math.BigInteger

data class Pixel(var r: Int, var g: Int, var b: Int)
var b3Seg: Int = 30
var b: MutableMap<String, Any?> = newBitmap(400, 300)
fun pixelFromRgb(rgb: Int): Pixel {
    var r: Int = (Math.floorMod((rgb / 65536), 256)).toInt()
    var g: Int = (Math.floorMod((rgb / 256), 256)).toInt()
    var b: Int = (Math.floorMod(rgb, 256)).toInt()
    return Pixel(r = r, g = g, b = b)
}

fun newBitmap(cols: Int, rows: Int): MutableMap<String, Any?> {
    var d: MutableList<MutableList<Pixel>> = mutableListOf<MutableList<Pixel>>()
    var y: Int = 0
    while (y < rows) {
        var row: MutableList<Pixel> = mutableListOf<Pixel>()
        var x: Int = 0
        while (x < cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(Pixel(r = 0, g = 0, b = 0)); _tmp } as MutableList<Pixel>
            x = x + 1
        }
        d = run { val _tmp = d.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Pixel>>
        y = y + 1
    }
    return mutableMapOf<String, Any?>("cols" to (cols), "rows" to (rows), "data" to (d))
}

fun setPx(b: MutableMap<String, Any?>, x: Int, y: Int, p: Pixel): Unit {
    var cols: Int = (b)["cols"] as Int
    var rows: Int = (b)["rows"] as Int
    if ((((((x >= 0) && (x < cols) as Boolean)) && (y >= 0) as Boolean)) && (y < rows)) {
        ((b)["data"] as Any?[y]!![x]) = p
    }
}

fun fill(b: MutableMap<String, Any?>, p: Pixel): Unit {
    var cols: Int = (b)["cols"] as Int
    var rows: Int = (b)["rows"] as Int
    var y: Int = 0
    while (y < rows) {
        var x: Int = 0
        while (x < cols) {
            ((b)["data"] as Any?[y]!![x]) = p
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
        setPx(b, x0, y0, p)
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

fun bezier3(b: MutableMap<String, Any?>, x1: Int, y1: Int, x2: Int, y2: Int, x3: Int, y3: Int, x4: Int, y4: Int, p: Pixel): Unit {
    var px: MutableList<Int> = mutableListOf<Int>()
    var py: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i <= b3Seg) {
        px = run { val _tmp = px.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        py = run { val _tmp = py.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    var fx1: Double = x1.toDouble()
    var fy1: Double = y1.toDouble()
    var fx2: Double = x2.toDouble()
    var fy2: Double = y2.toDouble()
    var fx3: Double = x3.toDouble()
    var fy3: Double = y3.toDouble()
    var fx4: Double = x4.toDouble()
    var fy4: Double = y4.toDouble()
    i = 0
    while (i <= b3Seg) {
        var d: Double = i.toDouble() / b3Seg.toDouble()
        var a: Double = 1.0 - d
        var bcoef: Double = a * a
        var ccoef: Double = d * d
        var a2: Double = a * bcoef
        var b2: Double = (3.0 * bcoef) * d
        var c2: Double = (3.0 * a) * ccoef
        var d2: Double = ccoef * d
        (px[i]) = ((((a2 * fx1) + (b2 * fx2)) + (c2 * fx3)) + (d2 * fx4)).toInt()
        (py[i]) = ((((a2 * fy1) + (b2 * fy2)) + (c2 * fy3)) + (d2 * fy4)).toInt()
        i = i + 1
    }
    var x0: Int = px[0]!!
    var y0: Int = py[0]!!
    i = 1
    while (i <= b3Seg) {
        var x: Int = px[i]!!
        var y: Int = py[i]!!
        line(b, x0, y0, x, y, p)
        x0 = x
        y0 = y
        i = i + 1
    }
}

fun main() {
    fillRgb(b, 16773055)
    bezier3(b, 20, 200, 700, 50, 0 - 300, 50, 380, 150, pixelFromRgb(4165615))
}
