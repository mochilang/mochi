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

data class Fibonacci(var sequence: MutableList<Int> = mutableListOf<Int>())
data class FibGetResult(var fib: Fibonacci = Fibonacci(sequence = mutableListOf<Int>()), var values: MutableList<Int> = mutableListOf<Int>())
fun create_fibonacci(): Fibonacci {
    return Fibonacci(sequence = mutableListOf(0, 1))
}

fun fib_get(f: Fibonacci, index: Int): FibGetResult {
    var seq: MutableList<Int> = f.sequence
    while (seq.size < index) {
        var next: Int = (seq[seq.size - 1]!! + seq[seq.size - 2]!!).toInt()
        seq = run { val _tmp = seq.toMutableList(); _tmp.add(next); _tmp }
    }
    f.sequence = seq
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < index) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(seq[i]!!); _tmp }
        i = i + 1
    }
    return FibGetResult(fib = f, values = result)
}

fun user_main(): Unit {
    var fib: Fibonacci = create_fibonacci()
    var res: FibGetResult = fib_get(fib, 10)
    fib = res.fib
    println(res.values.toString())
    res = fib_get(fib, 5)
    fib = res.fib
    println(res.values.toString())
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
