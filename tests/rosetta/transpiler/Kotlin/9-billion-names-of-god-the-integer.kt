var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
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

var x: Int = 1
fun bigTrim(a: MutableList<Int>): MutableList<Int> {
    var a: MutableList<Int> = a
    var n: Int = a.size
    while ((n > 1) && (a[n - 1] == 0)) {
        a = a.subList(0, n - 1)
        n = n - 1
    }
    return a
}

fun bigFromInt(x: Int): MutableList<Int> {
    if (x == 0) {
        return mutableListOf(0)
    }
    var digits: MutableList<Int> = mutableListOf()
    var n: Int = x
    while (n > 0) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(n % 10); _tmp } as MutableList<Int>
        n = n / 10
    }
    return digits
}

fun bigAdd(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var carry: Int = 0
    var i: Int = 0
    while ((((i < a.size) || (i < b.size) as Boolean)) || (carry > 0)) {
        var av: Int = 0
        if (i < a.size) {
            av = a[i]
        }
        var bv: Int = 0
        if (i < b.size) {
            bv = b[i]
        }
        var s: Int = (av + bv) + carry
        res = run { val _tmp = res.toMutableList(); _tmp.add(s % 10); _tmp } as MutableList<Int>
        carry = s / 10
        i = i + 1
    }
    return bigTrim(res)
}

fun bigSub(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var borrow: Int = 0
    var i: Int = 0
    while (i < a.size) {
        var av: Int = a[i]
        var bv: Int = 0
        if (i < b.size) {
            bv = b[i]
        }
        var diff: Int = (av - bv) - borrow
        if (diff < 0) {
            diff = diff + 10
            borrow = 1
        } else {
            borrow = 0
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(diff); _tmp } as MutableList<Int>
        i = i + 1
    }
    return bigTrim(res)
}

fun bigToString(a: MutableList<Int>): String {
    var s: String = ""
    var i: Int = a.size - 1
    while (i >= 0) {
        s = s + (a[i]).toString()
        i = i - 1
    }
    return s
}

fun minInt(a: Int, b: Int): Int {
    if (a < b) {
        return a
    } else {
        return b
    }
}

fun cumu(n: Int): MutableList<MutableList<Int>> {
    var cache: MutableList<MutableList<MutableList<Int>>> = mutableListOf(mutableListOf(bigFromInt(1)))
    var y: Int = 1
    while (y <= n) {
        var row: MutableList<MutableList<Int>> = mutableListOf(bigFromInt(0))
        var x: Int = 1
        while (x <= y) {
            val _val: MutableList<Int> = cache[y - x][minInt(x, y - x)]
            row = run { val _tmp = row.toMutableList(); _tmp.add(bigAdd(row[row.size - 1], _val)); _tmp } as MutableList<MutableList<Int>>
            x = x + 1
        }
        cache = run { val _tmp = cache.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<MutableList<Int>>>
        y = y + 1
    }
    return cache[n]
}

fun row(n: Int): MutableList<String> {
    val e: MutableList<MutableList<Int>> = cumu(n)
    var out: MutableList<String> = mutableListOf()
    var i: Int = 0
    while (i < n) {
        val diff: MutableList<Int> = bigSub(e[i + 1], e[i])
        out = run { val _tmp = out.toMutableList(); _tmp.add(bigToString(diff)); _tmp } as MutableList<String>
        i = i + 1
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("rows:")
        while (x < 11) {
            val r: MutableList<String> = row(x)
            var line: String = ""
            var i: Int = 0
            while (i < r.size) {
                line = ((line + " ") + r[i]) + " "
                i = i + 1
            }
            println(line)
            x = x + 1
        }
        println("")
        println("sums:")
        for (num in mutableListOf(23, 123, 1234)) {
            val r: MutableList<MutableList<Int>> = cumu(num)
            println((num.toString() + " ") + bigToString((r[r.size - 1]) as MutableList<Int>))
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
