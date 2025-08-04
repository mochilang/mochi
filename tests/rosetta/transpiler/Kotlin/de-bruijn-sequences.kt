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

fun dbRec(k: Int, n: Int, t: Int, p: Int, a: MutableList<Int>, seq: MutableList<Int>): MutableList<Int> {
    var seq: MutableList<Int> = seq
    if (t > n) {
        if ((Math.floorMod(n, p)) == 0) {
            var j: Int = 1
            while (j <= p) {
                seq = run { val _tmp = seq.toMutableList(); _tmp.add(a[j]!!); _tmp }
                j = j + 1
            }
        }
    } else {
        a[t] = a[t - p]!!
        seq = dbRec(k, n, t + 1, p, a, seq)
        var j: BigInteger = (a[t - p]!! + 1).toBigInteger()
        while (j.compareTo((k).toBigInteger()) < 0) {
            a[t] = (j.toInt())
            seq = dbRec(k, n, t + 1, t, a, seq)
            j = j.add((1).toBigInteger())
        }
    }
    return seq
}

fun deBruijn(k: Int, n: Int): String {
    var digits: String = "0123456789"
    var alphabet: String = digits
    if (k < 10) {
        alphabet = digits.substring(0, k)
    }
    var a: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < (k * n)) {
        a = run { val _tmp = a.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var seq: MutableList<Int> = mutableListOf<Int>()
    seq = dbRec(k, n, 1, 1, a, seq)
    var b: String = ""
    var idx: Int = 0
    while (idx < seq.size) {
        b = b + alphabet[seq[idx]!!].toString()
        idx = idx + 1
    }
    b = b + b.substring(0, n - 1)
    return b
}

fun allDigits(s: String): Boolean {
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if ((ch < "0") || (ch > "9")) {
            return false
        }
        i = i + 1
    }
    return true
}

fun parseIntStr(str: String): Int {
    var n: Int = 0
    var i: Int = 0
    while (i < str.length) {
        n = (n * 10) + ((str.substring(i, i + 1).toInt()))
        i = i + 1
    }
    return n
}

fun validate(db: String): Unit {
    var le: Int = db.length
    var found: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < 10000) {
        found = run { val _tmp = found.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var j: Int = 0
    while (j < (le - 3)) {
        var s: String = db.substring(j, j + 4)
        if (((allDigits(s)) as Boolean)) {
            var n: Int = Integer.parseInt(s, 10)
            found[n] = found[n]!! + 1
        }
        j = j + 1
    }
    var errs: MutableList<String> = mutableListOf<String>()
    var k: Int = 0
    while (k < 10000) {
        if (found[k]!! == 0) {
            errs = run { val _tmp = errs.toMutableList(); _tmp.add(("    PIN number " + padLeft(k, 4)) + " missing"); _tmp }
        } else {
            if (found[k]!! > 1) {
                errs = run { val _tmp = errs.toMutableList(); _tmp.add(((("    PIN number " + padLeft(k, 4)) + " occurs ") + (found[k]!!).toString()) + " times"); _tmp }
            }
        }
        k = k + 1
    }
    var lerr: Int = errs.size
    if (lerr == 0) {
        println("  No errors found")
    } else {
        var pl: String = "s"
        if (lerr == 1) {
            pl = ""
        }
        println(((("  " + lerr.toString()) + " error") + pl) + " found:")
        var msg: String = joinStr(errs, "\n")
        println(msg)
    }
}

fun padLeft(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = "0" + s
    }
    return s
}

fun joinStr(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]!!
        i = i + 1
    }
    return res
}

fun reverse(s: String): String {
    var out: String = ""
    var i: BigInteger = (s.length - 1).toBigInteger()
    while (i.compareTo((0).toBigInteger()) >= 0) {
        out = out + s.substring((i).toInt(), (i.add((1).toBigInteger())).toInt())
        i = i.subtract((1).toBigInteger())
    }
    return out
}

fun user_main(): Unit {
    var db: String = deBruijn(10, 4)
    var le: Int = db.length
    println("The length of the de Bruijn sequence is " + le.toString())
    println("\nThe first 130 digits of the de Bruijn sequence are:")
    println(db.substring(0, 130))
    println("\nThe last 130 digits of the de Bruijn sequence are:")
    println(db.substring(le - 130, db.length))
    println("\nValidating the de Bruijn sequence:")
    validate(db)
    println("\nValidating the reversed de Bruijn sequence:")
    var dbr: String = reverse(db)
    validate(dbr)
    db = (db.substring(0, 4443) + ".") + db.substring(4444, db.length)
    println("\nValidating the overlaid de Bruijn sequence:")
    validate(db)
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
