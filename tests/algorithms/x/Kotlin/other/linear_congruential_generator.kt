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

data class LCG(var multiplier: Int = 0, var increment: Int = 0, var modulo: Int = 0, var seed: Int = 0)
var lcg: LCG = make_lcg(1664525, 1013904223, (4294967296L.toInt()), _now())
var i: Int = (0).toInt()
fun make_lcg(multiplier: Int, increment: Int, modulo: Int, seed: Int): LCG {
    return LCG(multiplier = multiplier, increment = increment, modulo = modulo, seed = seed)
}

fun next_number(lcg: LCG): Int {
    lcg.seed = Math.floorMod(((lcg.multiplier * lcg.seed) + lcg.increment), lcg.modulo)
    return lcg.seed
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i < 5) {
            println(next_number(lcg).toString())
            i = (i + 1).toInt()
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
