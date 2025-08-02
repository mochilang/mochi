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
data class Bitmap(var w: Int = 0, var h: Int = 0, var max: Int = 0, var data: MutableList<MutableList<Pixel>> = mutableListOf<MutableList<Pixel>>())
var ppmtxt: String = (((((("P3\n" + "# feep.ppm\n") + "4 4\n") + "15\n") + " 0  0  0    0  0  0    0  0  0   15  0 15\n") + " 0  0  0    0 15  7    0  0  0    0  0  0\n") + " 0  0  0    0  0  0    0 15  7    0  0  0\n") + "15  0 15    0  0  0    0  0  0    0  0  0\n"
fun newBitmap(w: Int, h: Int, max: Int): Bitmap {
    var rows: MutableList<MutableList<Pixel>> = mutableListOf<MutableList<Pixel>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Pixel> = mutableListOf<Pixel>()
        var x: Int = 0
        while (x < w) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(Pixel(R = 0, G = 0, B = 0)); _tmp }
            x = x + 1
        }
        rows = run { val _tmp = rows.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return Bitmap(w = w, h = h, max = max, data = rows)
}

fun setPx(b: Bitmap, x: Int, y: Int, p: Pixel): Unit {
    var rows: MutableList<MutableList<Pixel>> = b.data
    var row: MutableList<Pixel> = rows[y]!!
    row[x] = p
    rows[y] = row
    b.data = rows
}

fun getPx(b: Bitmap, x: Int, y: Int): Pixel {
    return ((((b.data)[y]!!) as MutableList<Pixel>))[x]!!
}

fun splitLines(s: String): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == "\n") {
            out = run { val _tmp = out.toMutableList(); _tmp.add(cur); _tmp }
            cur = ""
        } else {
            cur = cur + ch
        }
        i = i + 1
    }
    out = run { val _tmp = out.toMutableList(); _tmp.add(cur); _tmp }
    return out
}

fun splitWS(s: String): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if ((((((ch == " ") || (ch == "\t") as Boolean)) || (ch == "\r") as Boolean)) || (ch == "\n")) {
            if (cur.length > 0) {
                out = run { val _tmp = out.toMutableList(); _tmp.add(cur); _tmp }
                cur = ""
            }
        } else {
            cur = cur + ch
        }
        i = i + 1
    }
    if (cur.length > 0) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(cur); _tmp }
    }
    return out
}

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + (digits)[str.substring(i, i + 1)] as Int
        i = i + 1
    }
    if ((neg as Boolean)) {
        n = 0 - n
    }
    return n
}

fun tokenize(s: String): MutableList<String> {
    var lines: MutableList<String> = splitLines(s)
    var toks: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < lines.size) {
        var line: String = lines[i]!!
        if ((line.length > 0) && (line.substring(0, 1) == "#")) {
            i = i + 1
            continue
        }
        var parts: MutableList<String> = splitWS(line)
        var j: Int = 0
        while (j < parts.size) {
            toks = run { val _tmp = toks.toMutableList(); _tmp.add(parts[j]!!); _tmp }
            j = j + 1
        }
        i = i + 1
    }
    return toks
}

fun readP3(text: String): Bitmap {
    var toks: MutableList<String> = tokenize(text)
    if (toks.size < 4) {
        return newBitmap(0, 0, 0)
    }
    if (toks[0]!! != "P3") {
        return newBitmap(0, 0, 0)
    }
    var w: Int = parseIntStr(toks[1]!!)
    var h: Int = parseIntStr(toks[2]!!)
    var maxv: Int = parseIntStr(toks[3]!!)
    var idx: Int = 4
    var bm: Bitmap = newBitmap(w, h, maxv)
    var y: BigInteger = (h - 1).toBigInteger()
    while (y.compareTo((0).toBigInteger()) >= 0) {
        var x: Int = 0
        while (x < w) {
            var r: Int = parseIntStr(toks[idx]!!)
            var g: Int = parseIntStr(toks[idx + 1]!!)
            var b: Int = parseIntStr(toks[idx + 2]!!)
            setPx(bm, x, (y.toInt()), Pixel(R = r, G = g, B = b))
            idx = idx + 3
            x = x + 1
        }
        y = y.subtract((1).toBigInteger())
    }
    return bm
}

fun toGrey(b: Bitmap): Unit {
    var h: Int = b.h
    var w: Int = b.w
    var m: Int = 0
    var y: Int = 0
    while (y < h) {
        var x: Int = 0
        while (x < w) {
            var p: Pixel = getPx(b, x, y)
            var l: BigInteger = ((((p.R * 2126) + (p.G * 7152)) + (p.B * 722)) / 10000).toBigInteger()
            if (l.compareTo((b.max).toBigInteger()) > 0) {
                l = ((b.max).toBigInteger())
            }
            setPx(b, x, y, Pixel(R = (l.toInt()), G = (l.toInt()), B = (l.toInt())))
            if (l.compareTo((m).toBigInteger()) > 0) {
                m = (l.toInt())
            }
            x = x + 1
        }
        y = y + 1
    }
    b.max = m
}

fun pad(n: Int, w: Int): String {
    var s: String = n.toString()
    while (s.length < w) {
        s = " " + s
    }
    return s
}

fun writeP3(b: Bitmap): String {
    var h: Int = b.h
    var w: Int = b.w
    var max: Int = b.max
    var digits: Int = max.toString().length
    var out: String = ((((("P3\n# generated from Bitmap.writeppmp3\n" + w.toString()) + " ") + h.toString()) + "\n") + max.toString()) + "\n"
    var y: BigInteger = (h - 1).toBigInteger()
    while (y.compareTo((0).toBigInteger()) >= 0) {
        var line: String = ""
        var x: Int = 0
        while (x < w) {
            var p: Pixel = getPx(b, x, (y.toInt()))
            line = (((((line + "   ") + pad(p.R, digits)) + " ") + pad(p.G, digits)) + " ") + pad(p.B, digits)
            x = x + 1
        }
        out = (out + line) + "\n"
        y = y.subtract((1).toBigInteger())
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Original Colour PPM file")
        println(ppmtxt)
        var bm: Bitmap = readP3(ppmtxt)
        println("Grey PPM:")
        toGrey(bm)
        var out: String = writeP3(bm)
        println(out)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
