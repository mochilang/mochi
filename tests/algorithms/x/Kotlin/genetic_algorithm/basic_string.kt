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

data class PairString(var first: String = "", var second: String = "")
fun evaluate(item: String, target: String): Int {
    var score: Int = (0).toInt()
    var i: Int = (0).toInt()
    while ((i < item.length) && (i < target.length)) {
        if (item.substring(i, i + 1) == target.substring(i, i + 1)) {
            score = score + 1
        }
        i = i + 1
    }
    return score
}

fun crossover(parent1: String, parent2: String): PairString {
    var cut: Int = (parent1.length / 2).toInt()
    var child1: String = parent1.substring(0, cut) + parent2.substring(cut, parent2.length)
    var child2: String = parent2.substring(0, cut) + parent1.substring(cut, parent1.length)
    return PairString(first = child1, second = child2)
}

fun mutate(child: String, genes: MutableList<String>): String {
    if (child.length == 0) {
        return child
    }
    var gene: String = genes[0]!!
    return child.substring(0, child.length - 1) + gene
}

fun user_main(): Unit {
    println(evaluate("Helxo Worlx", "Hello World").toString())
    var pair: PairString = crossover("123456", "abcdef")
    println(pair.first)
    println(pair.second)
    var mut: String = mutate("123456", mutableListOf("A", "B", "C", "D", "E", "F"))
    println(mut)
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
