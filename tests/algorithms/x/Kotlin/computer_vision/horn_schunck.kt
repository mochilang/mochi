import java.math.BigInteger

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

fun round_int(x: Double): Int {
    if (x >= 0.0) {
        return ((x + 0.5).toInt())
    }
    return ((x - 0.5).toInt())
}

fun zeros(rows: Int, cols: Int): MutableList<MutableList<Double>> {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = 0
    while (i < rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = 0
        while (j < cols) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun warp(image: MutableList<MutableList<Double>>, h_flow: MutableList<MutableList<Double>>, v_flow: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var h: Int = image.size
    var w: Int = (image[0]!!).size
    var out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var x: Int = 0
        while (x < w) {
            var sx: Int = x - round_int((((h_flow[y]!!) as MutableList<Double>))[x]!!)
            var sy: Int = y - round_int((((v_flow[y]!!) as MutableList<Double>))[x]!!)
            if ((((((sx >= 0) && (sx < w) as Boolean)) && (sy >= 0) as Boolean)) && (sy < h)) {
                row = run { val _tmp = row.toMutableList(); _tmp.add((((image[sy]!!) as MutableList<Double>))[sx]!!); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            }
            x = x + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return out
}

fun convolve(img: MutableList<MutableList<Double>>, ker: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var h: Int = img.size
    var w: Int = (img[0]!!).size
    var kh: Int = ker.size
    var kw: Int = (ker[0]!!).size
    var py: Int = kh / 2
    var px: Int = kw / 2
    var out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var x: Int = 0
        while (x < w) {
            var s: Double = 0.0
            var ky: Int = 0
            while (ky < kh) {
                var kx: Int = 0
                while (kx < kw) {
                    var iy: Int = (y + ky) - py
                    var ix: Int = (x + kx) - px
                    if ((((((iy >= 0) && (iy < h) as Boolean)) && (ix >= 0) as Boolean)) && (ix < w)) {
                        s = s + ((((img[iy]!!) as MutableList<Double>))[ix]!! * (((ker[ky]!!) as MutableList<Double>))[kx]!!)
                    }
                    kx = kx + 1
                }
                ky = ky + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(s); _tmp }
            x = x + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return out
}

fun horn_schunck(image0: MutableList<MutableList<Double>>, image1: MutableList<MutableList<Double>>, num_iter: Int, alpha: Double): MutableList<MutableList<MutableList<Double>>> {
    var h: Int = image0.size
    var w: Int = (image0[0]!!).size
    var u: MutableList<MutableList<Double>> = zeros(h, w)
    var v: MutableList<MutableList<Double>> = zeros(h, w)
    var kernel_x: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0 - 0.25, 0.25), mutableListOf(0.0 - 0.25, 0.25))
    var kernel_y: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0 - 0.25, 0.0 - 0.25), mutableListOf(0.25, 0.25))
    var kernel_t: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.25, 0.25), mutableListOf(0.25, 0.25))
    var laplacian: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0833333333333, 0.166666666667, 0.0833333333333), mutableListOf(0.166666666667, 0.0, 0.166666666667), mutableListOf(0.0833333333333, 0.166666666667, 0.0833333333333))
    var it: Int = 0
    while (it < num_iter) {
        var warped: MutableList<MutableList<Double>> = warp(image0, u, v)
        var dx1: MutableList<MutableList<Double>> = convolve(warped, kernel_x)
        var dx2: MutableList<MutableList<Double>> = convolve(image1, kernel_x)
        var dy1: MutableList<MutableList<Double>> = convolve(warped, kernel_y)
        var dy2: MutableList<MutableList<Double>> = convolve(image1, kernel_y)
        var dt1: MutableList<MutableList<Double>> = convolve(warped, kernel_t)
        var dt2: MutableList<MutableList<Double>> = convolve(image1, kernel_t)
        var avg_u: MutableList<MutableList<Double>> = convolve(u, laplacian)
        var avg_v: MutableList<MutableList<Double>> = convolve(v, laplacian)
        var y: Int = 0
        while (y < h) {
            var x: Int = 0
            while (x < w) {
                var dx: Double = (((dx1[y]!!) as MutableList<Double>))[x]!! + (((dx2[y]!!) as MutableList<Double>))[x]!!
                var dy: Double = (((dy1[y]!!) as MutableList<Double>))[x]!! + (((dy2[y]!!) as MutableList<Double>))[x]!!
                var dt: Double = (((dt1[y]!!) as MutableList<Double>))[x]!! - (((dt2[y]!!) as MutableList<Double>))[x]!!
                var au: Double = (((avg_u[y]!!) as MutableList<Double>))[x]!!
                var av: Double = (((avg_v[y]!!) as MutableList<Double>))[x]!!
                var numer: Double = ((dx * au) + (dy * av)) + dt
                var denom: Double = ((alpha * alpha) + (dx * dx)) + (dy * dy)
                var upd: Double = numer / denom
                _listSet(u[y]!!, x, au - (dx * upd))
                _listSet(v[y]!!, x, av - (dy * upd))
                x = x + 1
            }
            y = y + 1
        }
        it = it + 1
    }
    return mutableListOf(u, v)
}

fun print_matrix(mat: MutableList<MutableList<Double>>): Unit {
    var y: Int = 0
    while (y < mat.size) {
        var row: MutableList<Double> = mat[y]!!
        var x: Int = 0
        var line: String = ""
        while (x < row.size) {
            line = line + round_int(row[x]!!).toString()
            if ((x + 1) < row.size) {
                line = line + " "
            }
            x = x + 1
        }
        println(line)
        y = y + 1
    }
}

fun user_main(): Unit {
    var image0: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0, 2.0), mutableListOf(0.0, 0.0, 2.0))
    var image1: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 2.0, 0.0), mutableListOf(0.0, 2.0, 0.0))
    var flows: MutableList<MutableList<MutableList<Double>>> = horn_schunck(image0, image1, 20, 0.1)
    var u: MutableList<MutableList<Double>> = flows[0]!!
    var v: MutableList<MutableList<Double>> = flows[1]!!
    print_matrix(u)
    println("---")
    print_matrix(v)
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
