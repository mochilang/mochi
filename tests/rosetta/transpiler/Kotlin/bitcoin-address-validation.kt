fun _sha256(bs: List<Int>): MutableList<Int> {
    val md = java.security.MessageDigest.getInstance("SHA-256")
    val arr = ByteArray(bs.size)
    for (i in bs.indices) arr[i] = bs[i].toByte()
    val hash = md.digest(arr)
    val res = mutableListOf<Int>()
    for (b in hash) res.add((b.toInt() and 0xff))
    return res
}

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

fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun set58(addr: String): MutableList<Int> {
    var tmpl: String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    var a: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < 25) {
        a = run { val _tmp = a.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    var idx: Int = 0
    while (idx < addr.length) {
        var ch: String = addr.substring(idx, idx + 1)
        var c: Int = tmpl.indexOf(ch)
        if (c < 0) {
            return mutableListOf<Int>()
        }
        var j: Int = 24
        while (j >= 0) {
            c = c + (58 * a[j]!!)
            a[j] = Math.floorMod(c, 256)
            c = (c / 256).toInt()
            j = j - 1
        }
        if (c > 0) {
            return mutableListOf<Int>()
        }
        idx = idx + 1
    }
    return a
}

fun doubleSHA256(bs: MutableList<Int>): MutableList<Int> {
    var first: MutableList<Int> = _sha256(bs)
    return _sha256(first)
}

fun computeChecksum(a: MutableList<Int>): MutableList<Int> {
    var hash: MutableList<Int> = doubleSHA256(a.subList(0, 21))
    return hash.subList(0, 4)
}

fun validA58(addr: String): Boolean {
    var a: MutableList<Int> = set58(addr)
    if (a.size != 25) {
        return false
    }
    if (a[0]!! != 0) {
        return false
    }
    var sum: MutableList<Int> = computeChecksum(a)
    var i: Int = 0
    while (i < 4) {
        if (a[21 + i]!! != sum[i]!!) {
            return false
        }
        i = i + 1
    }
    return true
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(validA58("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i").toString())
        println(validA58("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j").toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
