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

var image: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2, 3), mutableListOf(4, 5, 6), mutableListOf(7, 8, 9))
var boxes: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.25, 0.25, 0.5, 0.5), mutableListOf(1.0, 0.75, 0.75, 0.5, 0.5))
fun flip_horizontal_image(img: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var flipped: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < img.size) {
        var row: MutableList<Int> = img[i]!!
        var j: BigInteger = ((row.size - 1).toBigInteger())
        var new_row: MutableList<Int> = mutableListOf<Int>()
        while (j.compareTo((0).toBigInteger()) >= 0) {
            new_row = run { val _tmp = new_row.toMutableList(); _tmp.add(row[(j).toInt()]!!); _tmp }
            j = j.subtract((1).toBigInteger())
        }
        flipped = run { val _tmp = flipped.toMutableList(); _tmp.add(new_row); _tmp }
        i = i + 1
    }
    return flipped
}

fun flip_vertical_image(img: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var flipped: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: BigInteger = ((img.size - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        flipped = run { val _tmp = flipped.toMutableList(); _tmp.add(img[(i).toInt()]!!); _tmp }
        i = i.subtract((1).toBigInteger())
    }
    return flipped
}

fun flip_horizontal_boxes(boxes: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = 0
    while (i < boxes.size) {
        var b: MutableList<Double> = boxes[i]!!
        var x_new: Double = 1.0 - b[1]!!
        result = run { val _tmp = result.toMutableList(); _tmp.add(mutableListOf(b[0]!!, x_new, b[2]!!, b[3]!!, b[4]!!)); _tmp }
        i = i + 1
    }
    return result
}

fun flip_vertical_boxes(boxes: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var result: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = 0
    while (i < boxes.size) {
        var b: MutableList<Double> = boxes[i]!!
        var y_new: Double = 1.0 - b[2]!!
        result = run { val _tmp = result.toMutableList(); _tmp.add(mutableListOf(b[0]!!, b[1]!!, y_new, b[3]!!, b[4]!!)); _tmp }
        i = i + 1
    }
    return result
}

fun print_image(img: MutableList<MutableList<Int>>): Unit {
    var i: Int = 0
    while (i < img.size) {
        var row: MutableList<Int> = img[i]!!
        var j: Int = 0
        var line: String = ""
        while (j < row.size) {
            line = (line + (row[j]!!).toString()) + " "
            j = j + 1
        }
        println(line)
        i = i + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Original image:")
        print_image(image)
        println(boxes.toString())
        println("Horizontal flip:")
        var h_img: MutableList<MutableList<Int>> = flip_horizontal_image(image)
        var h_boxes: MutableList<MutableList<Double>> = flip_horizontal_boxes(boxes)
        print_image(h_img)
        println(h_boxes.toString())
        println("Vertical flip:")
        var v_img: MutableList<MutableList<Int>> = flip_vertical_image(image)
        var v_boxes: MutableList<MutableList<Double>> = flip_vertical_boxes(boxes)
        print_image(v_img)
        println(v_boxes.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
