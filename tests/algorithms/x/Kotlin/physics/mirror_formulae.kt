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

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun isclose(a: Double, b: Double, tolerance: Double): Boolean {
    return abs_float(a - b) < tolerance
}

fun focal_length(distance_of_object: Double, distance_of_image: Double): Double {
    if ((distance_of_object == 0.0) || (distance_of_image == 0.0)) {
        panic("Invalid inputs. Enter non zero values with respect to the sign convention.")
    }
    return 1.0 / ((1.0 / distance_of_object) + (1.0 / distance_of_image))
}

fun object_distance(focal_length: Double, distance_of_image: Double): Double {
    if ((distance_of_image == 0.0) || (focal_length == 0.0)) {
        panic("Invalid inputs. Enter non zero values with respect to the sign convention.")
    }
    return 1.0 / ((1.0 / focal_length) - (1.0 / distance_of_image))
}

fun image_distance(focal_length: Double, distance_of_object: Double): Double {
    if ((distance_of_object == 0.0) || (focal_length == 0.0)) {
        panic("Invalid inputs. Enter non zero values with respect to the sign convention.")
    }
    return 1.0 / ((1.0 / focal_length) - (1.0 / distance_of_object))
}

fun test_focal_length(): Unit {
    var f1: Double = focal_length(10.0, 20.0)
    if (!isclose(f1, 6.66666666666666, 0.00000001)) {
        panic("focal_length test1 failed")
    }
    var f2: Double = focal_length(9.5, 6.7)
    if (!isclose(f2, 3.929012346, 0.00000001)) {
        panic("focal_length test2 failed")
    }
}

fun test_object_distance(): Unit {
    var u1: Double = object_distance(30.0, 20.0)
    if (!isclose(u1, 0.0 - 60.0, 0.00000001)) {
        panic("object_distance test1 failed")
    }
    var u2: Double = object_distance(10.5, 11.7)
    if (!isclose(u2, 102.375, 0.00000001)) {
        panic("object_distance test2 failed")
    }
}

fun test_image_distance(): Unit {
    var v1: Double = image_distance(10.0, 40.0)
    if (!isclose(v1, 13.33333333, 0.00000001)) {
        panic("image_distance test1 failed")
    }
    var v2: Double = image_distance(1.5, 6.7)
    if (!isclose(v2, 1.932692308, 0.00000001)) {
        panic("image_distance test2 failed")
    }
}

fun user_main(): Unit {
    test_focal_length()
    test_object_distance()
    test_image_distance()
    println(focal_length(10.0, 20.0).toString())
    println(object_distance(30.0, 20.0).toString())
    println(image_distance(10.0, 40.0).toString())
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
