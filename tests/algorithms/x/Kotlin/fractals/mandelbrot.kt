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

data class RGB(var r: Int = 0, var g: Int = 0, var b: Int = 0)
var img1: MutableList<MutableList<RGB>> = get_image(10, 10, 0.0 - 0.6, 0.0, 3.2, 50, true)
fun round_int(x: Double): Int {
    return ((x + 0.5).toInt())
}

fun hsv_to_rgb(h: Double, s: Double, v: Double): RGB {
    var i: Int = (((h * 6.0).toInt())).toInt()
    var f: Double = (h * 6.0) - ((i.toDouble()))
    var p: Double = v * (1.0 - s)
    var q: Double = v * (1.0 - (f * s))
    var t: Double = v * (1.0 - ((1.0 - f) * s))
    var mod: Int = (Math.floorMod(i, 6)).toInt()
    var r: Double = 0.0
    var g: Double = 0.0
    var b: Double = 0.0
    if (mod == 0) {
        r = v
        g = t
        b = p
    } else {
        if (mod == 1) {
            r = q
            g = v
            b = p
        } else {
            if (mod == 2) {
                r = p
                g = v
                b = t
            } else {
                if (mod == 3) {
                    r = p
                    g = q
                    b = v
                } else {
                    if (mod == 4) {
                        r = t
                        g = p
                        b = v
                    } else {
                        r = v
                        g = p
                        b = q
                    }
                }
            }
        }
    }
    return RGB(r = round_int(r * 255.0), g = round_int(g * 255.0), b = round_int(b * 255.0))
}

fun get_distance(x: Double, y: Double, max_step: Int): Double {
    var a: Double = x
    var b: Double = y
    var step: Int = (0 - 1).toInt()
    while (step < (max_step - 1)) {
        step = step + 1
        var a_new: Double = ((a * a) - (b * b)) + x
        b = ((2.0 * a) * b) + y
        a = a_new
        if (((a * a) + (b * b)) > 4.0) {
            break
        }
    }
    return ((step.toDouble())) / (((max_step - 1).toDouble()))
}

fun get_black_and_white_rgb(distance: Double): RGB {
    if (distance == 1.0) {
        return RGB(r = 0, g = 0, b = 0)
    } else {
        return RGB(r = 255, g = 255, b = 255)
    }
}

fun get_color_coded_rgb(distance: Double): RGB {
    if (distance == 1.0) {
        return RGB(r = 0, g = 0, b = 0)
    } else {
        return hsv_to_rgb(distance, 1.0, 1.0)
    }
}

fun get_image(image_width: Int, image_height: Int, figure_center_x: Double, figure_center_y: Double, figure_width: Double, max_step: Int, use_distance_color_coding: Boolean): MutableList<MutableList<RGB>> {
    var img: MutableList<MutableList<RGB>> = mutableListOf<MutableList<RGB>>()
    var figure_height: Double = (figure_width / ((image_width.toDouble()))) * ((image_height.toDouble()))
    var image_y: Int = (0).toInt()
    while (image_y < image_height) {
        var row: MutableList<RGB> = mutableListOf<RGB>()
        var image_x: Int = (0).toInt()
        while (image_x < image_width) {
            var fx: Double = figure_center_x + (((((image_x.toDouble())) / ((image_width.toDouble()))) - 0.5) * figure_width)
            var fy: Double = figure_center_y + (((((image_y.toDouble())) / ((image_height.toDouble()))) - 0.5) * figure_height)
            var distance: Double = get_distance(fx, fy, max_step)
            var rgb: RGB = RGB(r = 0, g = 0, b = 0)
            if ((use_distance_color_coding as Boolean)) {
                rgb = get_color_coded_rgb(distance)
            } else {
                rgb = get_black_and_white_rgb(distance)
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(rgb); _tmp }
            image_x = image_x + 1
        }
        img = run { val _tmp = img.toMutableList(); _tmp.add(row); _tmp }
        image_y = image_y + 1
    }
    return img
}

fun rgb_to_string(c: RGB): String {
    return ((((("(" + c.r.toString()) + ", ") + c.g.toString()) + ", ") + c.b.toString()) + ")"
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(rgb_to_string((((img1[0]!!) as MutableList<RGB>))[0]!!))
        var img2: MutableList<MutableList<RGB>> = get_image(10, 10, 0.0 - 0.6, 0.0, 3.2, 50, false)
        println(rgb_to_string((((img2[0]!!) as MutableList<RGB>))[0]!!))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
