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

var rgb: MutableList<Int> = hsv_to_rgb(180.0, 0.5, 0.5)
fun absf(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun fmod(a: Double, b: Double): Double {
    return a - (b * (((a / b).toInt())))
}

fun roundf(x: Double): Int {
    if (x >= 0.0) {
        return ((x + 0.5).toInt())
    }
    return ((x - 0.5).toInt())
}

fun maxf(a: Double, b: Double, c: Double): Double {
    var m: Double = a
    if (b > m) {
        m = b
    }
    if (c > m) {
        m = c
    }
    return m
}

fun minf(a: Double, b: Double, c: Double): Double {
    var m: Double = a
    if (b < m) {
        m = b
    }
    if (c < m) {
        m = c
    }
    return m
}

fun hsv_to_rgb(hue: Double, saturation: Double, value: Double): MutableList<Int> {
    if ((hue < 0.0) || (hue > 360.0)) {
        println("hue should be between 0 and 360")
        return mutableListOf<Int>()
    }
    if ((saturation < 0.0) || (saturation > 1.0)) {
        println("saturation should be between 0 and 1")
        return mutableListOf<Int>()
    }
    if ((value < 0.0) || (value > 1.0)) {
        println("value should be between 0 and 1")
        return mutableListOf<Int>()
    }
    var chroma: Double = value * saturation
    var hue_section: Double = hue / 60.0
    var second_largest_component: Double = chroma * (1.0 - absf(fmod(hue_section, 2.0) - 1.0))
    var match_value: Double = value - chroma
    var red: Int = 0
    var green: Int = 0
    var blue: Int = 0
    if ((hue_section >= 0.0) && (hue_section <= 1.0)) {
        red = roundf(255.0 * (chroma + match_value))
        green = roundf(255.0 * (second_largest_component + match_value))
        blue = roundf(255.0 * match_value)
    } else {
        if ((hue_section > 1.0) && (hue_section <= 2.0)) {
            red = roundf(255.0 * (second_largest_component + match_value))
            green = roundf(255.0 * (chroma + match_value))
            blue = roundf(255.0 * match_value)
        } else {
            if ((hue_section > 2.0) && (hue_section <= 3.0)) {
                red = roundf(255.0 * match_value)
                green = roundf(255.0 * (chroma + match_value))
                blue = roundf(255.0 * (second_largest_component + match_value))
            } else {
                if ((hue_section > 3.0) && (hue_section <= 4.0)) {
                    red = roundf(255.0 * match_value)
                    green = roundf(255.0 * (second_largest_component + match_value))
                    blue = roundf(255.0 * (chroma + match_value))
                } else {
                    if ((hue_section > 4.0) && (hue_section <= 5.0)) {
                        red = roundf(255.0 * (second_largest_component + match_value))
                        green = roundf(255.0 * match_value)
                        blue = roundf(255.0 * (chroma + match_value))
                    } else {
                        red = roundf(255.0 * (chroma + match_value))
                        green = roundf(255.0 * match_value)
                        blue = roundf(255.0 * (second_largest_component + match_value))
                    }
                }
            }
        }
    }
    return mutableListOf(red, green, blue)
}

fun rgb_to_hsv(red: Int, green: Int, blue: Int): MutableList<Double> {
    if ((red < 0) || (red > 255)) {
        println("red should be between 0 and 255")
        return mutableListOf<Double>()
    }
    if ((green < 0) || (green > 255)) {
        println("green should be between 0 and 255")
        return mutableListOf<Double>()
    }
    if ((blue < 0) || (blue > 255)) {
        println("blue should be between 0 and 255")
        return mutableListOf<Double>()
    }
    var float_red: Double = red / 255.0
    var float_green: Double = green / 255.0
    var float_blue: Double = blue / 255.0
    var value: Double = maxf(float_red, float_green, float_blue)
    var min_val: Double = minf(float_red, float_green, float_blue)
    var chroma: Double = value - min_val
    var saturation: Double = (if (value == 0.0) 0.0 else chroma / value.toDouble())
    var hue: Double = 0.0
    if (chroma == 0.0) {
        hue = 0.0
    } else {
        if (value == float_red) {
            hue = 60.0 * (0.0 + ((float_green - float_blue) / chroma))
        } else {
            if (value == float_green) {
                hue = 60.0 * (2.0 + ((float_blue - float_red) / chroma))
            } else {
                hue = 60.0 * (4.0 + ((float_red - float_green) / chroma))
            }
        }
    }
    hue = fmod(hue + 360.0, 360.0)
    return mutableListOf(hue, saturation, value)
}

fun approximately_equal_hsv(hsv1: MutableList<Double>, hsv2: MutableList<Double>): Boolean {
    var check_hue: Boolean = absf(hsv1[0]!! - hsv2[0]!!) < 0.2
    var check_saturation: Boolean = absf(hsv1[1]!! - hsv2[1]!!) < 0.002
    var check_value: Boolean = absf(hsv1[2]!! - hsv2[2]!!) < 0.002
    return ((((check_hue && check_saturation as Boolean)) && check_value) as Boolean)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(rgb.toString())
        var hsv: MutableList<Double> = rgb_to_hsv(64, 128, 128)
        println(hsv.toString())
        println(approximately_equal_hsv(hsv, mutableListOf(180.0, 0.5, 0.5)).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
