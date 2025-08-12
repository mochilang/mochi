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

fun focal_length_of_lens(object_distance_from_lens: Double, image_distance_from_lens: Double): Double {
    if ((object_distance_from_lens == 0.0) || (image_distance_from_lens == 0.0)) {
        panic("Invalid inputs. Enter non zero values with respect to the sign convention.")
    }
    return 1.0 / ((1.0 / image_distance_from_lens) - (1.0 / object_distance_from_lens))
}

fun object_distance(focal_length_of_lens: Double, image_distance_from_lens: Double): Double {
    if ((image_distance_from_lens == 0.0) || (focal_length_of_lens == 0.0)) {
        panic("Invalid inputs. Enter non zero values with respect to the sign convention.")
    }
    return 1.0 / ((1.0 / image_distance_from_lens) - (1.0 / focal_length_of_lens))
}

fun image_distance(focal_length_of_lens: Double, object_distance_from_lens: Double): Double {
    if ((object_distance_from_lens == 0.0) || (focal_length_of_lens == 0.0)) {
        panic("Invalid inputs. Enter non zero values with respect to the sign convention.")
    }
    return 1.0 / ((1.0 / object_distance_from_lens) + (1.0 / focal_length_of_lens))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(focal_length_of_lens(10.0, 4.0).toString())
        println(focal_length_of_lens(2.7, 5.8).toString())
        println(object_distance(10.0, 40.0).toString())
        println(object_distance(6.2, 1.5).toString())
        println(image_distance(50.0, 40.0).toString())
        println(image_distance(5.3, 7.9).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
