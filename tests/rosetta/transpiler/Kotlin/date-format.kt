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

fun pad2(n: Int): String {
    if (n < 10) {
        return "0" + n.toString()
    }
    return n.toString()
}

fun weekdayName(z: Int): String {
    var names: MutableList<String> = mutableListOf("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    return names[Math.floorMod((z + 4), 7)]!!
}

fun user_main(): Unit {
    var ts: Int = ((_now() / 1000000000).toInt())
    var days: Int = ((ts / 86400).toInt())
    var z: BigInteger = (days + 719468).toBigInteger()
    var era: Int = ((z.divide((146097).toBigInteger())).toInt())
    var doe: BigInteger = z.subtract((era * 146097).toBigInteger())
    var yoe: BigInteger = (((doe.subtract((doe.divide((1460).toBigInteger())))).add((doe.divide((36524).toBigInteger())))).subtract((doe.divide((146096).toBigInteger())))).divide(((365.toInt())).toBigInteger())
    var y: BigInteger = yoe.add((era * 400).toBigInteger())
    var doy: BigInteger = doe.subtract(((((365).toBigInteger().multiply((yoe))).add((yoe.divide((4).toBigInteger())))).subtract((yoe.divide((100).toBigInteger())))))
    var mp: BigInteger = (((5).toBigInteger().multiply((doy))).add((2).toBigInteger())).divide(((153.toInt())).toBigInteger())
    var d: Int = (((doy.subtract(((((153).toBigInteger().multiply((mp))).add((2).toBigInteger())).divide(((5.toInt())).toBigInteger())))).add((1).toBigInteger())).toInt())
    var m: Int = ((mp.add((3).toBigInteger())).toInt())
    if (m > 12) {
        y = y.add((1).toBigInteger())
        m = m - 12
    }
    var iso: String = (((y.toString() + "-") + pad2(m)) + "-") + pad2(d)
    println(iso)
    var months: MutableList<String> = mutableListOf("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    var line: String = (((((weekdayName(days) + ", ") + months[m - 1]!!) + " ") + d.toString()) + ", ") + y.toString()
    println(line)
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
