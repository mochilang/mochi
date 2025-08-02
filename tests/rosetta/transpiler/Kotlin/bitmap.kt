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

fun Extent(b: Bitmap): MutableMap<String, Int> {
    return mutableMapOf<String, Int>("cols" to (b.cols), "rows" to (b.rows))
}

fun Fill(b: Bitmap, p: Pixel): Unit {
    var y: Int = 0
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

fun FillRgb(b: Bitmap, c: Int): Unit {
    Fill(b, pixelFromRgb(c))
}

fun SetPx(b: Bitmap, x: Int, y: Int, p: Pixel): Boolean {
    if ((((((x < 0) || (x >= b.cols) as Boolean)) || (y < 0) as Boolean)) || (y >= b.rows)) {
        return false
    }
    var px: MutableList<MutableList<Pixel>> = b.px
    var row: MutableList<Pixel> = px[y]!!
    row[x] = p
    px[y] = row
    b.px = px
    return true
}

fun SetPxRgb(b: Bitmap, x: Int, y: Int, c: Int): Boolean {
    return SetPx(b, x, y, pixelFromRgb(c))
}

fun GetPx(b: Bitmap, x: Int, y: Int): MutableMap<String, Any?> {
    if ((((((x < 0) || (x >= b.cols) as Boolean)) || (y < 0) as Boolean)) || (y >= b.rows)) {
        return (mutableMapOf<String, Boolean>("ok" to (false)) as MutableMap<String, Any?>)
    }
    var row: MutableList<Pixel> = (b.px)[y]!!
    return mutableMapOf<String, Any?>("ok" to (true), "pixel" to (row[x]!!))
}

fun GetPxRgb(b: Bitmap, x: Int, y: Int): MutableMap<String, Any?> {
    var r: MutableMap<String, Any?> = GetPx(b, x, y)
    if (!((r)["ok"]!! as Boolean)) {
        return (mutableMapOf<String, Boolean>("ok" to (false)) as MutableMap<String, Any?>)
    }
    return mutableMapOf<String, Any?>("ok" to (true), "rgb" to (rgbFromPixel((((r)["pixel"]!!) as Pixel))))
}

fun ppmSize(b: Bitmap): Int {
    var header: String = ((("P6\n# Creator: Rosetta Code http://rosettacode.org/\n" + b.cols.toString()) + " ") + b.rows.toString()) + "\n255\n"
    return header.length + ((3 * b.cols) * b.rows)
}

fun pixelStr(p: Pixel): String {
    return ((((("{" + p.R.toString()) + " ") + p.G.toString()) + " ") + p.B.toString()) + "}"
}

fun user_main(): Unit {
    var bm: Bitmap = NewBitmap(300, 240)
    FillRgb(bm, 16711680)
    SetPxRgb(bm, 10, 20, 255)
    SetPxRgb(bm, 20, 30, 0)
    SetPxRgb(bm, 30, 40, 1056816)
    var c1: MutableMap<String, Any?> = GetPx(bm, 0, 0)
    var c2: MutableMap<String, Any?> = GetPx(bm, 10, 20)
    var c3: MutableMap<String, Any?> = GetPx(bm, 30, 40)
    println((("Image size: " + bm.cols.toString()) + " × ") + bm.rows.toString())
    println(ppmSize(bm).toString() + " bytes when encoded as PPM.")
    if ((((c1)["ok"]!!) as Boolean)) {
        println("Pixel at (0,0) is " + pixelStr((((c1)["pixel"]!!) as Pixel)))
    }
    if ((((c2)["ok"]!!) as Boolean)) {
        println("Pixel at (10,20) is " + pixelStr((((c2)["pixel"]!!) as Pixel)))
    }
    if ((((c3)["ok"]!!) as Boolean)) {
        var p: Any? = (c3)["pixel"]!!
        var r16: Int = (((p) as Pixel).R) * 257
        var g16: Int = (((p) as Pixel).G) * 257
        var b16: Int = (((p) as Pixel).B) * 257
        println((((("Pixel at (30,40) has R=" + r16.toString()) + ", G=") + g16.toString()) + ", B=") + b16.toString())
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
