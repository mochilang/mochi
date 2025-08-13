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

data class FuzzySet(var name: String = "", var left_boundary: Double = 0.0, var peak: Double = 0.0, var right_boundary: Double = 0.0)
var sheru: FuzzySet = FuzzySet(name = "Sheru", left_boundary = 0.4, peak = 1.0, right_boundary = 0.6)
var siya: FuzzySet = FuzzySet(name = "Siya", left_boundary = 0.5, peak = 1.0, right_boundary = 0.7)
fun stringify(fs: FuzzySet): String {
    return ((((((fs.name + ": [") + fs.left_boundary.toString()) + ", ") + fs.peak.toString()) + ", ") + fs.right_boundary.toString()) + "]"
}

fun max2(a: Double, b: Double): Double {
    if (a > b) {
        return a
    }
    return b
}

fun min2(a: Double, b: Double): Double {
    if (a < b) {
        return a
    }
    return b
}

fun complement(fs: FuzzySet): FuzzySet {
    return FuzzySet(name = "¬" + fs.name, left_boundary = 1.0 - fs.right_boundary, peak = 1.0 - fs.left_boundary, right_boundary = 1.0 - fs.peak)
}

fun intersection(a: FuzzySet, b: FuzzySet): FuzzySet {
    return FuzzySet(name = (a.name + " ∩ ") + b.name, left_boundary = max2(a.left_boundary, b.left_boundary), peak = min2(a.right_boundary, b.right_boundary), right_boundary = (a.peak + b.peak) / 2.0)
}

fun union(a: FuzzySet, b: FuzzySet): FuzzySet {
    return FuzzySet(name = (a.name + " U ") + b.name, left_boundary = min2(a.left_boundary, b.left_boundary), peak = max2(a.right_boundary, b.right_boundary), right_boundary = (a.peak + b.peak) / 2.0)
}

fun membership(fs: FuzzySet, x: Double): Double {
    if ((x <= fs.left_boundary) || (x >= fs.right_boundary)) {
        return 0.0
    }
    if ((fs.left_boundary < x) && (x <= fs.peak)) {
        return (x - fs.left_boundary) / (fs.peak - fs.left_boundary)
    }
    if ((fs.peak < x) && (x < fs.right_boundary)) {
        return (fs.right_boundary - x) / (fs.right_boundary - fs.peak)
    }
    return 0.0
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(stringify(sheru))
        println(stringify(siya))
        var sheru_comp: FuzzySet = complement(sheru)
        println(stringify(sheru_comp))
        var inter: FuzzySet = intersection(siya, sheru)
        println(stringify(inter))
        println("Sheru membership 0.5: " + membership(sheru, 0.5).toString())
        println("Sheru membership 0.6: " + membership(sheru, 0.6).toString())
        var uni: FuzzySet = union(siya, sheru)
        println(stringify(uni))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
