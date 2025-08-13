fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

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

var valid_colors: MutableList<String> = mutableListOf("Black", "Brown", "Red", "Orange", "Yellow", "Green", "Blue", "Violet", "Grey", "White", "Gold", "Silver")
var significant_figures_color_values: MutableMap<String, Int> = mutableMapOf<String, Int>("Black" to (0), "Brown" to (1), "Red" to (2), "Orange" to (3), "Yellow" to (4), "Green" to (5), "Blue" to (6), "Violet" to (7), "Grey" to (8), "White" to (9))
var multiplier_color_values: MutableMap<String, Double> = mutableMapOf<String, Double>("Black" to (1.0), "Brown" to (10.0), "Red" to (100.0), "Orange" to (1000.0), "Yellow" to (10000.0), "Green" to (100000.0), "Blue" to (1000000.0), "Violet" to (10000000.0), "Grey" to (100000000.0), "White" to (1000000000.0), "Gold" to (0.1), "Silver" to (0.01))
var tolerance_color_values: MutableMap<String, Double> = mutableMapOf<String, Double>("Brown" to (1.0), "Red" to (2.0), "Orange" to (0.05), "Yellow" to (0.02), "Green" to (0.5), "Blue" to (0.25), "Violet" to (0.1), "Grey" to (0.01), "Gold" to (5.0), "Silver" to (10.0))
var temperature_coeffecient_color_values: MutableMap<String, Int> = mutableMapOf<String, Int>("Black" to (250), "Brown" to (100), "Red" to (50), "Orange" to (15), "Yellow" to (25), "Green" to (20), "Blue" to (10), "Violet" to (5), "Grey" to (1))
fun contains(list: MutableList<String>, value: String): Boolean {
    for (c in list) {
        if (c == value) {
            return true
        }
    }
    return false
}

fun get_significant_digits(colors: MutableList<String>): Int {
    var digit: Int = (0).toInt()
    for (color in colors) {
        if (!(color in significant_figures_color_values)) {
            panic(color + " is not a valid color for significant figure bands")
        }
        digit = (digit * 10) + (significant_figures_color_values)[color] as Int
    }
    return digit
}

fun get_multiplier(color: String): Double {
    if (!(color in multiplier_color_values)) {
        panic(color + " is not a valid color for multiplier band")
    }
    return (multiplier_color_values)[color] as Double
}

fun get_tolerance(color: String): Double {
    if (!(color in tolerance_color_values)) {
        panic(color + " is not a valid color for tolerance band")
    }
    return (tolerance_color_values)[color] as Double
}

fun get_temperature_coeffecient(color: String): Int {
    if (!(color in temperature_coeffecient_color_values)) {
        panic(color + " is not a valid color for temperature coeffecient band")
    }
    return (temperature_coeffecient_color_values)[color] as Int
}

fun get_band_type_count(total: Int, typ: String): Int {
    if (total == 3) {
        if (typ == "significant") {
            return 2
        }
        if (typ == "multiplier") {
            return 1
        }
        panic(typ + " is not valid for a 3 band resistor")
    } else {
        if (total == 4) {
            if (typ == "significant") {
                return 2
            }
            if (typ == "multiplier") {
                return 1
            }
            if (typ == "tolerance") {
                return 1
            }
            panic(typ + " is not valid for a 4 band resistor")
        } else {
            if (total == 5) {
                if (typ == "significant") {
                    return 3
                }
                if (typ == "multiplier") {
                    return 1
                }
                if (typ == "tolerance") {
                    return 1
                }
                panic(typ + " is not valid for a 5 band resistor")
            } else {
                if (total == 6) {
                    if (typ == "significant") {
                        return 3
                    }
                    if (typ == "multiplier") {
                        return 1
                    }
                    if (typ == "tolerance") {
                        return 1
                    }
                    if (typ == "temp_coeffecient") {
                        return 1
                    }
                    panic(typ + " is not valid for a 6 band resistor")
                } else {
                    panic(_numToStr(total) + " is not a valid number of bands")
                }
            }
        }
    }
}

fun check_validity(number_of_bands: Int, colors: MutableList<String>): Boolean {
    if ((number_of_bands < 3) || (number_of_bands > 6)) {
        panic("Invalid number of bands. Resistor bands must be 3 to 6")
    }
    if (number_of_bands != colors.size) {
        panic(((("Expecting " + _numToStr(number_of_bands)) + " colors, provided ") + _numToStr(colors.size)) + " colors")
    }
    for (color in colors) {
        if (!contains(valid_colors, color)) {
            panic(color + " is not a valid color")
        }
    }
    return true
}

fun calculate_resistance(number_of_bands: Int, color_code_list: MutableList<String>): String {
    check_validity(number_of_bands, color_code_list)
    var sig_count: Int = (get_band_type_count(number_of_bands, "significant")).toInt()
    var significant_colors: MutableList<String> = _sliceList(color_code_list, 0, sig_count)
    var significant_digits: Int = (get_significant_digits(significant_colors)).toInt()
    var multiplier_color: String = color_code_list[sig_count]!!
    var multiplier: Double = get_multiplier(multiplier_color)
    var tolerance: Double = 20.0
    if (number_of_bands >= 4) {
        var tolerance_color: String = color_code_list[sig_count + 1]!!
        tolerance = get_tolerance(tolerance_color)
    }
    var temp_coeff: Int = (0).toInt()
    if (number_of_bands == 6) {
        var temp_color: String = color_code_list[sig_count + 2]!!
        temp_coeff = get_temperature_coeffecient(temp_color)
    }
    var resistance_value: Double = multiplier * (significant_digits).toDouble()
    var resistance_str: String = _numToStr(resistance_value)
    if (resistance_value == ((resistance_value.toInt())).toDouble()) {
        resistance_str = _numToStr((resistance_value.toInt()))
    }
    var answer: String = ((resistance_str + "Ω ±") + _numToStr(tolerance)) + "% "
    if (temp_coeff != 0) {
        answer = (answer + _numToStr(temp_coeff)) + " ppm/K"
    }
    return answer
}

fun test_3_band_resistor(): Unit {
    expect(calculate_resistance(3, mutableListOf("Black", "Blue", "Orange")) == "6000Ω ±20% ")
}

fun test_4_band_resistor(): Unit {
    expect(calculate_resistance(4, mutableListOf("Orange", "Green", "Blue", "Gold")) == "35000000Ω ±5% ")
}

fun test_5_band_resistor(): Unit {
    expect(calculate_resistance(5, mutableListOf("Violet", "Brown", "Grey", "Silver", "Green")) == "7.18Ω ±0.5% ")
}

fun test_6_band_resistor(): Unit {
    expect(calculate_resistance(6, mutableListOf("Red", "Green", "Blue", "Yellow", "Orange", "Grey")) == "2560000Ω ±0.05% 1 ppm/K")
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        test_3_band_resistor()
        test_4_band_resistor()
        test_5_band_resistor()
        test_6_band_resistor()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
