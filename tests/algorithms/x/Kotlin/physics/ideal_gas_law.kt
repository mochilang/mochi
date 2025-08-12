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

var UNIVERSAL_GAS_CONSTANT: Double = 8.314462
fun pressure_of_gas_system(moles: Double, kelvin: Double, volume: Double): Double {
    if ((((moles < 0) || (kelvin < 0) as Boolean)) || (volume < 0)) {
        panic("Invalid inputs. Enter positive value.")
    }
    return ((moles * kelvin) * UNIVERSAL_GAS_CONSTANT) / volume
}

fun volume_of_gas_system(moles: Double, kelvin: Double, pressure: Double): Double {
    if ((((moles < 0) || (kelvin < 0) as Boolean)) || (pressure < 0)) {
        panic("Invalid inputs. Enter positive value.")
    }
    return ((moles * kelvin) * UNIVERSAL_GAS_CONSTANT) / pressure
}

fun temperature_of_gas_system(moles: Double, volume: Double, pressure: Double): Double {
    if ((((moles < 0) || (volume < 0) as Boolean)) || (pressure < 0)) {
        panic("Invalid inputs. Enter positive value.")
    }
    return (pressure * volume) / (moles * UNIVERSAL_GAS_CONSTANT)
}

fun moles_of_gas_system(kelvin: Double, volume: Double, pressure: Double): Double {
    if ((((kelvin < 0) || (volume < 0) as Boolean)) || (pressure < 0)) {
        panic("Invalid inputs. Enter positive value.")
    }
    return (pressure * volume) / (kelvin * UNIVERSAL_GAS_CONSTANT)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(pressure_of_gas_system(2.0, 100.0, 5.0))
        println(volume_of_gas_system(0.5, 273.0, 0.004))
        println(temperature_of_gas_system(2.0, 100.0, 5.0))
        println(moles_of_gas_system(100.0, 5.0, 10.0))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
