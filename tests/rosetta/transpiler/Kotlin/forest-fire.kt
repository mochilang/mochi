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

var rows: Int = 20
var cols: Int = 30
var p: Double = 0.01
var f: Double = 0.001
var board: MutableList<MutableList<String>> = newBoard()
fun repeat(ch: String, n: Int): String {
    var s: String = ""
    var i: Int = 0
    while (i < n) {
        s = s + ch
        i = i + 1
    }
    return s
}

fun chance(prob: Double): Boolean {
    var threshold: Int = (prob * 1000.0).toInt()
    return (Math.floorMod(_now(), 1000)) < threshold
}

fun newBoard(): MutableList<MutableList<String>> {
    var b: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var r: Int = 0
    while (r < rows) {
        var row: MutableList<String> = mutableListOf<String>()
        var c: Int = 0
        while (c < cols) {
            if ((Math.floorMod(_now(), 2)) == 0) {
                row = run { val _tmp = row.toMutableList(); _tmp.add("T"); _tmp } as MutableList<String>
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(" "); _tmp } as MutableList<String>
            }
            c = c + 1
        }
        b = run { val _tmp = b.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<String>>
        r = r + 1
    }
    return b
}

fun step(src: MutableList<MutableList<String>>): MutableList<MutableList<String>> {
    var dst: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var r: Int = 0
    while (r < rows) {
        var row: MutableList<String> = mutableListOf<String>()
        var c: Int = 0
        while (c < cols) {
            var cell: String = ((src[r]!!) as MutableList<String>)[c]!!
            var next: String = cell
            if (cell == "#") {
                next = " "
            } else {
                if (cell == "T") {
                    var burning: Boolean = false
                    var dr: Int = 0 - 1
                    while (dr <= 1) {
                        var dc: Int = 0 - 1
                        while (dc <= 1) {
                            if ((dr != 0) || (dc != 0)) {
                                var rr: BigInteger = (r + dr).toBigInteger()
                                var cc: BigInteger = (c + dc).toBigInteger()
                                if ((((((rr.compareTo(0.toBigInteger()) >= 0) && (rr.compareTo(rows.toBigInteger()) < 0) as Boolean)) && (cc.compareTo(0.toBigInteger()) >= 0) as Boolean)) && (cc.compareTo(cols.toBigInteger()) < 0)) {
                                    if (((src[(rr).toInt()]!!) as MutableList<String>)[(cc).toInt()]!! == "#") {
                                        burning = true
                                    }
                                }
                            }
                            dc = dc + 1
                        }
                        dr = dr + 1
                    }
                    if (burning || chance(f)) {
                        next = "#"
                    }
                } else {
                    if ((chance(p)) as Boolean) {
                        next = "T"
                    }
                }
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(next); _tmp } as MutableList<String>
            c = c + 1
        }
        dst = run { val _tmp = dst.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<String>>
        r = r + 1
    }
    return dst
}

fun printBoard(b: MutableList<MutableList<String>>): Unit {
    println(repeat("__", cols) + "\n\n")
    var r: Int = 0
    while (r < rows) {
        var line: String = ""
        var c: Int = 0
        while (c < cols) {
            var cell: String = ((b[r]!!) as MutableList<String>)[c]!!
            if (cell == " ") {
                line = line + "  "
            } else {
                line = (line + " ") + cell
            }
            c = c + 1
        }
        println(line + "\n")
        r = r + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        printBoard(board)
        board = step(board)
        printBoard(board)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
