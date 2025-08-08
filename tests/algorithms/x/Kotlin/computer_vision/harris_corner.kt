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

var img: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 1, 1, 1, 1), mutableListOf(1, 255, 255, 255, 1), mutableListOf(1, 255, 0, 255, 1), mutableListOf(1, 255, 255, 255, 1), mutableListOf(1, 1, 1, 1, 1))
var corners: MutableList<MutableList<Int>> = harris(img, 0.04, 3, 10000000000.0)
fun zeros(h: Int, w: Int): MutableList<MutableList<Double>> {
    var m: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var x: Int = 0
        while (x < w) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            x = x + 1
        }
        m = run { val _tmp = m.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return m
}

fun gradient(img: MutableList<MutableList<Int>>): MutableList<MutableList<MutableList<Double>>> {
    var h: Int = img.size
    var w: Int = (img[0]!!).size
    var dx: MutableList<MutableList<Double>> = zeros(h, w)
    var dy: MutableList<MutableList<Double>> = zeros(h, w)
    var y: Int = 1
    while (y < (h - 1)) {
        var x: Int = 1
        while (x < (w - 1)) {
            _listSet(dx[y]!!, x, ((((((img[y]!!) as MutableList<Int>))[x + 1]!!).toDouble())) - ((((((img[y]!!) as MutableList<Int>))[x - 1]!!).toDouble())))
            _listSet(dy[y]!!, x, ((((((img[y + 1]!!) as MutableList<Int>))[x]!!).toDouble())) - ((((((img[y - 1]!!) as MutableList<Int>))[x]!!).toDouble())))
            x = x + 1
        }
        y = y + 1
    }
    return mutableListOf<MutableList<MutableList<Double>>>(dx, dy)
}

fun harris(img: MutableList<MutableList<Int>>, k: Double, window: Int, thresh: Double): MutableList<MutableList<Int>> {
    var h: Int = img.size
    var w: Int = (img[0]!!).size
    var grads: MutableList<MutableList<MutableList<Double>>> = gradient(img)
    var dx: MutableList<MutableList<Double>> = grads[0]!!
    var dy: MutableList<MutableList<Double>> = grads[1]!!
    var ixx: MutableList<MutableList<Double>> = zeros(h, w)
    var iyy: MutableList<MutableList<Double>> = zeros(h, w)
    var ixy: MutableList<MutableList<Double>> = zeros(h, w)
    var y: Int = 0
    while (y < h) {
        var x: Int = 0
        while (x < w) {
            var gx: Double = (((dx[y]!!) as MutableList<Double>))[x]!!
            var gy: Double = (((dy[y]!!) as MutableList<Double>))[x]!!
            _listSet(ixx[y]!!, x, gx * gx)
            _listSet(iyy[y]!!, x, gy * gy)
            _listSet(ixy[y]!!, x, gx * gy)
            x = x + 1
        }
        y = y + 1
    }
    var offset: Int = window / 2
    var corners: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    y = offset
    while (y < (h - offset)) {
        var x: Int = offset
        while (x < (w - offset)) {
            var wxx: Double = 0.0
            var wyy: Double = 0.0
            var wxy: Double = 0.0
            var yy: BigInteger = ((y - offset).toBigInteger())
            while (yy.compareTo((y + offset).toBigInteger()) <= 0) {
                var xx: BigInteger = ((x - offset).toBigInteger())
                while (xx.compareTo((x + offset).toBigInteger()) <= 0) {
                    wxx = wxx + (((ixx[(yy).toInt()]!!) as MutableList<Double>))[(xx).toInt()]!!
                    wyy = wyy + (((iyy[(yy).toInt()]!!) as MutableList<Double>))[(xx).toInt()]!!
                    wxy = wxy + (((ixy[(yy).toInt()]!!) as MutableList<Double>))[(xx).toInt()]!!
                    xx = xx.add((1).toBigInteger())
                }
                yy = yy.add((1).toBigInteger())
            }
            var det: Double = (wxx * wyy) - (wxy * wxy)
            var trace: Double = wxx + wyy
            var r: Double = det - (k * (trace * trace))
            if (r > thresh) {
                corners = run { val _tmp = corners.toMutableList(); _tmp.add(mutableListOf<Int>(x, y)); _tmp }
            }
            x = x + 1
        }
        y = y + 1
    }
    return corners
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(corners)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
