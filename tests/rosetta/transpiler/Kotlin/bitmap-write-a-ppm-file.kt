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

data class Colour(var R: Int = 0, var G: Int = 0, var B: Int = 0)
data class Bitmap(var width: Int = 0, var height: Int = 0, var pixels: MutableList<MutableList<Colour>> = mutableListOf<MutableList<Colour>>())
fun newBitmap(w: Int, h: Int, c: Colour): Bitmap {
    var rows: MutableList<MutableList<Colour>> = mutableListOf<MutableList<Colour>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Colour> = mutableListOf<Colour>()
        var x: Int = 0
        while (x < w) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(c); _tmp }
            x = x + 1
        }
        rows = run { val _tmp = rows.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return Bitmap(width = w, height = h, pixels = rows)
}

fun setPixel(b: Bitmap, x: Int, y: Int, c: Colour): Unit {
    var rows: MutableList<MutableList<Colour>> = b.pixels
    var row: MutableList<Colour> = rows[y]!!
    row[x] = c
    rows[y] = row
    b.pixels = rows
}

fun fillRect(b: Bitmap, x: Int, y: Int, w: Int, h: Int, c: Colour): Unit {
    var yy: Int = y
    while (yy < (y + h)) {
        var xx: Int = x
        while (xx < (x + w)) {
            setPixel(b, xx, yy, c)
            xx = xx + 1
        }
        yy = yy + 1
    }
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun writePPMP3(b: Bitmap): String {
    var maxv: Int = 0
    var y: Int = 0
    while (y < b.height) {
        var x: Int = 0
        while (x < b.width) {
            var p: Colour = ((((b.pixels)[y]!!) as MutableList<Colour>))[x]!!
            if (p.R > maxv) {
                maxv = p.R
            }
            if (p.G > maxv) {
                maxv = p.G
            }
            if (p.B > maxv) {
                maxv = p.B
            }
            x = x + 1
        }
        y = y + 1
    }
    var out: String = ((((("P3\n# generated from Bitmap.writeppmp3\n" + b.width.toString()) + " ") + b.height.toString()) + "\n") + maxv.toString()) + "\n"
    var numsize: Int = maxv.toString().length
    y = b.height - 1
    while (y >= 0) {
        var line: String = ""
        var x: Int = 0
        while (x < b.width) {
            var p: Colour = ((((b.pixels)[y]!!) as MutableList<Colour>))[x]!!
            line = (((((line + "   ") + pad(p.R, numsize)) + " ") + pad(p.G, numsize)) + " ") + pad(p.B, numsize)
            x = x + 1
        }
        out = out + line
        if (y > 0) {
            out = out + "\n"
        } else {
            out = out + "\n"
        }
        y = y - 1
    }
    return out
}

fun user_main(): Unit {
    var black: Colour = Colour(R = 0, G = 0, B = 0)
    var white: Colour = Colour(R = 255, G = 255, B = 255)
    var bm: Bitmap = newBitmap(4, 4, black)
    fillRect(bm, 1, 0, 1, 2, white)
    setPixel(bm, 3, 3, Colour(R = 127, G = 0, B = 63))
    var ppm: String = writePPMP3(bm)
    println(ppm)
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
