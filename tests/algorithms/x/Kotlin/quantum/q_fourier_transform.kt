fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

fun to_bits(n: Int, width: Int): String {
    var res: String = ""
    var num: Int = (n).toInt()
    var w: Int = (width).toInt()
    while (w > 0) {
        res = (Math.floorMod(num, 2)).toString() + res
        num = num / 2
        w = w - 1
    }
    return res
}

fun quantum_fourier_transform(number_of_qubits: Int): MutableMap<String, Int> {
    if (number_of_qubits <= 0) {
        panic("number of qubits must be > 0.")
    }
    if (number_of_qubits > 10) {
        panic("number of qubits too large to simulate(>10).")
    }
    var shots: Int = (10000).toInt()
    var states: Int = (1).toInt()
    var p: Int = (0).toInt()
    while (p < number_of_qubits) {
        states = states * 2
        p = p + 1
    }
    var per_state: Int = (shots / states).toInt()
    var counts: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var i: Int = (0).toInt()
    while (i < states) {
        (counts)[to_bits(i, number_of_qubits)] = per_state
        i = i + 1
    }
    return counts
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Total count for quantum fourier transform state is: " + quantum_fourier_transform(3).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
