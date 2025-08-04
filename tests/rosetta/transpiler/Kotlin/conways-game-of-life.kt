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

data class Field(var s: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>(), var w: Int = 0, var h: Int = 0)
data class Life(var a: Field = Field(s = mutableListOf<MutableList<Boolean>>(), w = 0, h = 0), var b: Field = Field(s = mutableListOf<MutableList<Boolean>>(), w = 0, h = 0), var w: Int = 0, var h: Int = 0)
var seed: Int = 1
fun randN(n: Int): Int {
    seed = Math.floorMod(((seed * 1664525) + 1013904223), 2147483647)
    return Math.floorMod(seed, n)
}

fun newField(w: Int, h: Int): Field {
    var rows: MutableList<MutableList<Boolean>> = mutableListOf<MutableList<Boolean>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Boolean> = mutableListOf<Boolean>()
        var x: Int = 0
        while (x < w) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(false); _tmp }
            x = x + 1
        }
        rows = run { val _tmp = rows.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return Field(s = rows, w = w, h = h)
}

fun setCell(f: Field, x: Int, y: Int, b: Boolean): Unit {
    var rows: MutableList<MutableList<Boolean>> = f.s
    var row: MutableList<Boolean> = rows[y]!!
    row[x] = b
    rows[y] = row
    f.s = rows
}

fun state(f: Field, x: Int, y: Int): Boolean {
    var y: Int = y
    var x: Int = x
    while (y < 0) {
        y = y + f.h
    }
    while (x < 0) {
        x = x + f.w
    }
    return ((((f.s)[Math.floorMod(y, f.h)]!!) as MutableList<Boolean>))[Math.floorMod(x, f.w)]!!
}

fun nextState(f: Field, x: Int, y: Int): Boolean {
    var count: Int = 0
    var dy: Int = 0 - 1
    while (dy <= 1) {
        var dx: Int = 0 - 1
        while (dx <= 1) {
            if ((!(((dx == 0) && (dy == 0)) as Boolean) as Boolean) && state(f, x + dx, y + dy)) {
                count = count + 1
            }
            dx = dx + 1
        }
        dy = dy + 1
    }
    return (((count == 3) || (((count == 2) && state(f, x, y) as Boolean))) as Boolean)
}

fun newLife(w: Int, h: Int): Life {
    var a: Field = newField(w, h)
    var i: Int = 0
    while (i < ((w * h) / 2)) {
        setCell(a, randN(w), randN(h), true)
        i = i + 1
    }
    return Life(a = a, b = newField(w, h), w = w, h = h)
}

fun step(l: Life): Unit {
    var y: Int = 0
    while (y < l.h) {
        var x: Int = 0
        while (x < l.w) {
            setCell(l.b, x, y, nextState(l.a, x, y))
            x = x + 1
        }
        y = y + 1
    }
    var tmp: Field = l.a
    l.a = l.b
    l.b = tmp
}

fun lifeString(l: Life): String {
    var out: String = ""
    var y: Int = 0
    while (y < l.h) {
        var x: Int = 0
        while (x < l.w) {
            if (((state(l.a, x, y)) as Boolean)) {
                out = out + "*"
            } else {
                out = out + " "
            }
            x = x + 1
        }
        out = out + "\n"
        y = y + 1
    }
    return out
}

fun user_main(): Unit {
    var l: Life = newLife(80, 15)
    var i: Int = 0
    while (i < 300) {
        step(l)
        println("\u000c")
        println(lifeString(l))
        i = i + 1
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
