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

val a: MutableList<Double> = mutableListOf(1.0, 0.0 - 0.00000000000000027756, 0.33333333, 0.0 - 0.0000000000000000185)
val b: MutableList<Double> = mutableListOf(0.16666667, 0.5, 0.5, 0.16666667)
val sig: MutableList<Double> = mutableListOf(0.0 - 0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, 0.0 - 0.662370894973, 0.0 - 1.00700480494, 0.0 - 0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, 0.0 - 0.2085993586, 0.0 - 0.172842103641, 0.0 - 0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589)
val res: MutableList<Double> = applyFilter(sig, a, b)
var k: Int = 0
fun applyFilter(input: MutableList<Double>, a: MutableList<Double>, b: MutableList<Double>): MutableList<Double> {
    var out: MutableList<Double> = mutableListOf()
    val scale: Double = 1.0 / a[0]
    var i: Int = 0
    while (i < input.size) {
        var tmp: Double = 0.0
        var j: Int = 0
        while ((j <= i) && (j < b.size)) {
            tmp = tmp + (b[j] * input[i - j])
            j = j + 1
        }
        j = 0
        while ((j < i) && ((j + 1) < a.size)) {
            tmp = tmp - (a[j + 1] * out[(i - j) - 1])
            j = j + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(tmp * scale); _tmp } as MutableList<Double>
        i = i + 1
    }
    return out
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (k < res.size) {
            println(res[k])
            k = k + 1
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
