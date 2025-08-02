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

data class Pixel(var R: Int = 0, var G: Int = 0, var B: Int = 0)
data class Bitmap(var cols: Int = 0, var rows: Int = 0, var px: MutableList<MutableList<Pixel>> = mutableListOf<MutableList<Pixel>>())
fun pixelFromRgb(c: Int): Pixel {
    var r: BigInteger = (Math.floorMod((((c / 65536).toInt())), 256)).toBigInteger()
    var g: BigInteger = (Math.floorMod((((c / 256).toInt())), 256)).toBigInteger()
    var b: BigInteger = (Math.floorMod(c, 256)).toBigInteger()
    return Pixel(R = (r.toInt()), G = (g.toInt()), B = (b.toInt()))
}

fun rgbFromPixel(p: Pixel): Int {
    return ((p.R * 65536) + (p.G * 256)) + p.B
}

fun NewBitmap(x: Int, y: Int): Bitmap {
    var data: MutableList<MutableList<Pixel>> = mutableListOf<MutableList<Pixel>>()
    var row: Int = 0
    while (row < y) {
        var r: MutableList<Pixel> = mutableListOf<Pixel>()
        var col: Int = 0
        while (col < x) {
            r = run { val _tmp = r.toMutableList(); _tmp.add(Pixel(R = 0, G = 0, B = 0)); _tmp }
            col = col + 1
        }
        data = run { val _tmp = data.toMutableList(); _tmp.add(r); _tmp }
        row = row + 1
    }
    return Bitmap(cols = x, rows = y, px = data)
}

fun FillRgb(b: Bitmap, c: Int): Unit {
    var y: Int = 0
    var p: Pixel = pixelFromRgb(c)
    while (y < b.rows) {
        var x: Int = 0
        while (x < b.cols) {
            var px: MutableList<MutableList<Pixel>> = b.px
            var row: MutableList<Pixel> = px[y]!!
            row[x] = p
            px[y] = row
            b.px = px
            x = x + 1
        }
        y = y + 1
    }
}

fun SetPxRgb(b: Bitmap, x: Int, y: Int, c: Int): Boolean {
    if ((((((x < 0) || (x >= b.cols) as Boolean)) || (y < 0) as Boolean)) || (y >= b.rows)) {
        return false
    }
    var px: MutableList<MutableList<Pixel>> = b.px
    var row: MutableList<Pixel> = px[y]!!
    row[x] = pixelFromRgb(c)
    px[y] = row
    b.px = px
    return true
}

fun nextRand(seed: Int): Int {
    return Math.floorMod((((seed * 1664525) + 1013904223).toLong()), 2147483648L).toInt()
}

fun user_main(): Unit {
    var bm: Bitmap = NewBitmap(400, 300)
    FillRgb(bm, 12615744)
    var seed: Long = _now()
    var i: Int = 0
    while (i < 2000) {
        seed = ((nextRand((seed.toInt()))).toLong())
        var x: BigInteger = (Math.floorMod(seed, (400).toLong()).toInt()).toBigInteger()
        seed = ((nextRand((seed.toInt()))).toLong())
        var y: BigInteger = (Math.floorMod(seed, (300).toLong()).toInt()).toBigInteger()
        SetPxRgb(bm, (x.toInt()), (y.toInt()), 8405024)
        i = i + 1
    }
    var x: Int = 0
    while (x < 400) {
        var y: Int = 240
        while (y < 245) {
            SetPxRgb(bm, x, y, 8405024)
            y = y + 1
        }
        y = 260
        while (y < 265) {
            SetPxRgb(bm, x, y, 8405024)
            y = y + 1
        }
        x = x + 1
    }
    var y: Int = 0
    while (y < 300) {
        var x: Int = 80
        while (x < 85) {
            SetPxRgb(bm, x, y, 8405024)
            x = x + 1
        }
        x = 95
        while (x < 100) {
            SetPxRgb(bm, x, y, 8405024)
            x = x + 1
        }
        y = y + 1
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
