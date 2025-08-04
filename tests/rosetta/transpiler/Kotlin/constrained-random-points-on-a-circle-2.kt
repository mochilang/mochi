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

var nPts: Int = 100
var rMin: Int = 10
var rMax: Int = 15
var span: BigInteger = ((rMax + 1) + rMax).toBigInteger()
var poss: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
var min2: BigInteger = (rMin * rMin).toBigInteger()
var max2: BigInteger = (rMax * rMax).toBigInteger()
var y: Int = 0 - rMax
fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (y <= rMax) {
            var x: Int = 0 - rMax
            while (x <= rMax) {
                var r2: BigInteger = ((x * x) + (y * y)).toBigInteger()
                if ((r2.compareTo((min2)) >= 0) && (r2.compareTo((max2)) <= 0)) {
                    poss = run { val _tmp = poss.toMutableList(); _tmp.add(mutableListOf(x, y)); _tmp }
                }
                x = x + 1
            }
            y = y + 1
        }
        println(poss.size.toString() + " possible points")
        var rows: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
        var r: Int = 0
        while ((r).toBigInteger().compareTo((span)) < 0) {
            var row: MutableList<String> = mutableListOf<String>()
            var c: Int = 0
            while ((c).toBigInteger().compareTo((span.multiply((2).toBigInteger()))) < 0) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(" "); _tmp }
                c = c + 1
            }
            rows = run { val _tmp = rows.toMutableList(); _tmp.add(row); _tmp }
            r = r + 1
        }
        var u: Int = 0
        var seen: MutableMap<String, Boolean> = mutableMapOf<String, Boolean>()
        var n: Int = 0
        while (n < nPts) {
            var i: BigInteger = (Math.floorMod(_now(), poss.size)).toBigInteger()
            var x: Int = (((poss[(i).toInt()]!!) as MutableList<Int>))[0]!!
            var yy: Int = (((poss[(i).toInt()]!!) as MutableList<Int>))[1]!!
            var row: BigInteger = (yy + rMax).toBigInteger()
            var col: BigInteger = ((x + rMax) * 2).toBigInteger()
            (rows[(row).toInt()]!!)[(col).toInt()] = "*"
            var key: String = (row.toString() + ",") + col.toString()
            if (!(((seen)[key] ?: false) as? Boolean ?: false)) {
                (seen)[key] = true
                u = u + 1
            }
            n = n + 1
        }
        var i2: Int = 0
        while ((i2).toBigInteger().compareTo((span)) < 0) {
            var line: String = ""
            var j: Int = 0
            while ((j).toBigInteger().compareTo((span.multiply((2).toBigInteger()))) < 0) {
                line = line + (((rows[i2]!!) as MutableList<String>))[j]!!
                j = j + 1
            }
            println(line)
            i2 = i2 + 1
        }
        println(u.toString() + " unique points")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
