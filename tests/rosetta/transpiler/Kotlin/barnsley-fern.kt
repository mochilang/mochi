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

val xMin: Double = 0.0 - 2.182
val xMax: Double = 2.6558
val yMin: Double = 0.0
val yMax: Double = 9.9983
val width: Int = 60
val nIter: Int = 10000
val dx: Double = xMax - xMin
val dy: Double = yMax - yMin
val height: Int = ((width * dy) / dx).toInt()
var grid: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
var row: Int = 0
var seed: Int = 1
var x: Double = 0.0
var y: Double = 0.0
var ix: Int = ((width.toDouble() * (x - xMin)) / dx).toInt()
var iy: Int = ((height.toDouble() * (yMax - y)) / dy).toInt()
var i: Int = 0
fun randInt(s: Int, n: Int): MutableList<Int> {
    val next: Int = Math.floorMod(((s * 1664525) + 1013904223), 2147483647)
    return mutableListOf(next, Math.floorMod(next, n))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (row < height) {
            var line: MutableList<String> = mutableListOf<String>()
            var col: Int = 0
            while (col < width) {
                line = run { val _tmp = line.toMutableList(); _tmp.add(" "); _tmp } as MutableList<String>
                col = col + 1
            }
            grid = run { val _tmp = grid.toMutableList(); _tmp.add(line); _tmp } as MutableList<MutableList<String>>
            row = row + 1
        }
        if ((((((ix >= 0) && (ix < width) as Boolean)) && (iy >= 0) as Boolean)) && (iy < height)) {
            grid[iy][ix] = "*"
        }
        while (i < nIter) {
            var res: MutableList<Int> = randInt(seed, 100)
            seed = res[0]
            val r: Int = res[1]
            if (r < 85) {
                val nx: Double = (0.85 * x) + (0.04 * y)
                val ny: Double = (((0.0 - 0.04) * x) + (0.85 * y)) + 1.6
                x = nx
                y = ny
            } else {
                if (r < 92) {
                    val nx: Double = (0.2 * x) - (0.26 * y)
                    val ny: Double = ((0.23 * x) + (0.22 * y)) + 1.6
                    x = nx
                    y = ny
                } else {
                    if (r < 99) {
                        val nx: Double = ((0.0 - 0.15) * x) + (0.28 * y)
                        val ny: Double = ((0.26 * x) + (0.24 * y)) + 0.44
                        x = nx
                        y = ny
                    } else {
                        x = 0.0
                        y = 0.16 * y
                    }
                }
            }
            ix = ((width.toDouble() * (x - xMin)) / dx).toInt()
            iy = ((height.toDouble() * (yMax - y)) / dy).toInt()
            if ((((((ix >= 0) && (ix < width) as Boolean)) && (iy >= 0) as Boolean)) && (iy < height)) {
                grid[iy][ix] = "*"
            }
            i = i + 1
        }
        row = 0
        while (row < height) {
            var line: String = ""
            var col: Int = 0
            while (col < width) {
                line = line + grid[row][col]
                col = col + 1
            }
            println(line)
            row = row + 1
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
