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

var image: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 1.0, 1.0, 0.0, 0.0, 0.0), mutableListOf(0.0, 1.0, 1.0, 0.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 1.0, 1.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 1.0, 1.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 0.0, 0.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
var kernel: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 0.0, 0.0 - 1.0), mutableListOf(1.0, 0.0, 0.0 - 1.0), mutableListOf(1.0, 0.0, 0.0 - 1.0))
var conv: MutableList<MutableList<Double>> = conv2d(image, kernel)
var activated: MutableList<MutableList<Double>> = relu_matrix(conv)
var pooled: MutableList<MutableList<Double>> = max_pool2x2(activated)
var flat: MutableList<Double> = flatten(pooled)
var weights: MutableList<Double> = mutableListOf(0.5, 0.0 - 0.4, 0.3, 0.1)
var bias: Double = 0.0
var output: Double = dense(flat, weights, bias)
var probability: Double = sigmoid(output)
fun conv2d(image: MutableList<MutableList<Double>>, kernel: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows: Int = image.size
    var cols: Int = (image[0]!!).size
    var k: Int = kernel.size
    var output: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = 0
    while (i <= (rows - k)) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = 0
        while (j <= (cols - k)) {
            var sum: Double = 0.0
            var ki: Int = 0
            while (ki < k) {
                var kj: Int = 0
                while (kj < k) {
                    sum = sum + ((((image[i + ki]!!) as MutableList<Double>))[j + kj]!! * (((kernel[ki]!!) as MutableList<Double>))[kj]!!)
                    kj = kj + 1
                }
                ki = ki + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum); _tmp }
            j = j + 1
        }
        output = run { val _tmp = output.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return output
}

fun relu_matrix(m: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    for (row in m) {
        var new_row: MutableList<Double> = mutableListOf<Double>()
        for (v in row) {
            if (v > 0.0) {
                new_row = run { val _tmp = new_row.toMutableList(); _tmp.add(v); _tmp }
            } else {
                new_row = run { val _tmp = new_row.toMutableList(); _tmp.add(0.0); _tmp }
            }
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(new_row); _tmp }
    }
    return out
}

fun max_pool2x2(m: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows: Int = m.size
    var cols: Int = (m[0]!!).size
    var out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = 0
    while (i < rows) {
        var new_row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = 0
        while (j < cols) {
            var max_val: Double = (((m[i]!!) as MutableList<Double>))[j]!!
            if ((((m[i]!!) as MutableList<Double>))[j + 1]!! > max_val) {
                max_val = (((m[i]!!) as MutableList<Double>))[j + 1]!!
            }
            if ((((m[i + 1]!!) as MutableList<Double>))[j]!! > max_val) {
                max_val = (((m[i + 1]!!) as MutableList<Double>))[j]!!
            }
            if ((((m[i + 1]!!) as MutableList<Double>))[j + 1]!! > max_val) {
                max_val = (((m[i + 1]!!) as MutableList<Double>))[j + 1]!!
            }
            new_row = run { val _tmp = new_row.toMutableList(); _tmp.add(max_val); _tmp }
            j = j + 2
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(new_row); _tmp }
        i = i + 2
    }
    return out
}

fun flatten(m: MutableList<MutableList<Double>>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    for (row in m) {
        for (v in row) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(v); _tmp }
        }
    }
    return res
}

fun dense(inputs: MutableList<Double>, weights: MutableList<Double>, bias: Double): Double {
    var s: Double = bias
    var i: Int = 0
    while (i < inputs.size) {
        s = s + (inputs[i]!! * weights[i]!!)
        i = i + 1
    }
    return s
}

fun exp_approx(x: Double): Double {
    var sum: Double = 1.0
    var term: Double = 1.0
    var i: Int = 1
    while (i <= 10) {
        term = (term * x) / i
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun sigmoid(x: Double): Double {
    return 1.0 / (1.0 + exp_approx(0.0 - x))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        if (probability >= 0.5) {
            println("Abnormality detected")
        } else {
            println("Normal")
        }
        println("Probability:")
        println(probability)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
