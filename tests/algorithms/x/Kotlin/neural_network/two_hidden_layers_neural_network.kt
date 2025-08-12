fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class Network(var w1: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var w2: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var w3: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>())
fun exp_approx(x: Double): Double {
    var sum: Double = 1.0
    var term: Double = 1.0
    var i: Int = (1).toInt()
    while (i < 10) {
        term = (term * x) / ((i.toDouble()))
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun sigmoid(x: Double): Double {
    return 1.0 / (1.0 + exp_approx(0.0 - x))
}

fun sigmoid_derivative(x: Double): Double {
    return x * (1.0 - x)
}

fun new_network(): Network {
    return Network(w1 = mutableListOf(mutableListOf(0.1, 0.2, 0.3, 0.4), mutableListOf(0.5, 0.6, 0.7, 0.8), mutableListOf(0.9, 1.0, 1.1, 1.2)), w2 = mutableListOf(mutableListOf(0.1, 0.2, 0.3), mutableListOf(0.4, 0.5, 0.6), mutableListOf(0.7, 0.8, 0.9), mutableListOf(1.0, 1.1, 1.2)), w3 = mutableListOf(mutableListOf(0.1), mutableListOf(0.2), mutableListOf(0.3)))
}

fun feedforward(net: Network, input: MutableList<Double>): Double {
    var hidden1: MutableList<Double> = mutableListOf<Double>()
    var j: Int = (0).toInt()
    while (j < 4) {
        var sum1: Double = 0.0
        var i: Int = (0).toInt()
        while (i < 3) {
            sum1 = sum1 + (input[i]!! * ((((net.w1)[i]!!) as MutableList<Double>))[j]!!)
            i = i + 1
        }
        hidden1 = run { val _tmp = hidden1.toMutableList(); _tmp.add(sigmoid(sum1)); _tmp }
        j = j + 1
    }
    var hidden2: MutableList<Double> = mutableListOf<Double>()
    var k: Int = (0).toInt()
    while (k < 3) {
        var sum2: Double = 0.0
        var j2: Int = (0).toInt()
        while (j2 < 4) {
            sum2 = sum2 + (hidden1[j2]!! * ((((net.w2)[j2]!!) as MutableList<Double>))[k]!!)
            j2 = j2 + 1
        }
        hidden2 = run { val _tmp = hidden2.toMutableList(); _tmp.add(sigmoid(sum2)); _tmp }
        k = k + 1
    }
    var sum3: Double = 0.0
    var k2: Int = (0).toInt()
    while (k2 < 3) {
        sum3 = sum3 + (hidden2[k2]!! * ((((net.w3)[k2]!!) as MutableList<Double>))[0]!!)
        k2 = k2 + 1
    }
    var out: Double = sigmoid(sum3)
    return out
}

fun train(net: Network, inputs: MutableList<MutableList<Double>>, outputs: MutableList<Double>, iterations: Int): Unit {
    var iter: Int = (0).toInt()
    while (iter < iterations) {
        var s: Int = (0).toInt()
        while (s < inputs.size) {
            var inp: MutableList<Double> = inputs[s]!!
            var target: Double = outputs[s]!!
            var hidden1: MutableList<Double> = mutableListOf<Double>()
            var j: Int = (0).toInt()
            while (j < 4) {
                var sum1: Double = 0.0
                var i: Int = (0).toInt()
                while (i < 3) {
                    sum1 = sum1 + (inp[i]!! * ((((net.w1)[i]!!) as MutableList<Double>))[j]!!)
                    i = i + 1
                }
                hidden1 = run { val _tmp = hidden1.toMutableList(); _tmp.add(sigmoid(sum1)); _tmp }
                j = j + 1
            }
            var hidden2: MutableList<Double> = mutableListOf<Double>()
            var k: Int = (0).toInt()
            while (k < 3) {
                var sum2: Double = 0.0
                var j2: Int = (0).toInt()
                while (j2 < 4) {
                    sum2 = sum2 + (hidden1[j2]!! * ((((net.w2)[j2]!!) as MutableList<Double>))[k]!!)
                    j2 = j2 + 1
                }
                hidden2 = run { val _tmp = hidden2.toMutableList(); _tmp.add(sigmoid(sum2)); _tmp }
                k = k + 1
            }
            var sum3: Double = 0.0
            var k3: Int = (0).toInt()
            while (k3 < 3) {
                sum3 = sum3 + (hidden2[k3]!! * ((((net.w3)[k3]!!) as MutableList<Double>))[0]!!)
                k3 = k3 + 1
            }
            var output: Double = sigmoid(sum3)
            var error: Double = target - output
            var delta_output: Double = error * sigmoid_derivative(output)
            var new_w3: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
            var k4: Int = (0).toInt()
            while (k4 < 3) {
                var w3row: MutableList<Double> = (net.w3)[k4]!!
                _listSet(w3row, 0, w3row[0]!! + (hidden2[k4]!! * delta_output))
                new_w3 = run { val _tmp = new_w3.toMutableList(); _tmp.add(w3row); _tmp }
                k4 = k4 + 1
            }
            net.w3 = new_w3
            var delta_hidden2: MutableList<Double> = mutableListOf<Double>()
            var k5: Int = (0).toInt()
            while (k5 < 3) {
                var row: MutableList<Double> = (net.w3)[k5]!!
                var dh2: Double = (row[0]!! * delta_output) * sigmoid_derivative(hidden2[k5]!!)
                delta_hidden2 = run { val _tmp = delta_hidden2.toMutableList(); _tmp.add(dh2); _tmp }
                k5 = k5 + 1
            }
            var new_w2: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
            j = 0
            while (j < 4) {
                var w2row: MutableList<Double> = (net.w2)[j]!!
                var k6: Int = (0).toInt()
                while (k6 < 3) {
                    _listSet(w2row, k6, w2row[k6]!! + (hidden1[j]!! * delta_hidden2[k6]!!))
                    k6 = k6 + 1
                }
                new_w2 = run { val _tmp = new_w2.toMutableList(); _tmp.add(w2row); _tmp }
                j = j + 1
            }
            net.w2 = new_w2
            var delta_hidden1: MutableList<Double> = mutableListOf<Double>()
            j = 0
            while (j < 4) {
                var sumdh: Double = 0.0
                var k7: Int = (0).toInt()
                while (k7 < 3) {
                    var row2: MutableList<Double> = (net.w2)[j]!!
                    sumdh = sumdh + (row2[k7]!! * delta_hidden2[k7]!!)
                    k7 = k7 + 1
                }
                delta_hidden1 = run { val _tmp = delta_hidden1.toMutableList(); _tmp.add(sumdh * sigmoid_derivative(hidden1[j]!!)); _tmp }
                j = j + 1
            }
            var new_w1: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
            var i2: Int = (0).toInt()
            while (i2 < 3) {
                var w1row: MutableList<Double> = (net.w1)[i2]!!
                j = 0
                while (j < 4) {
                    _listSet(w1row, j, w1row[j]!! + (inp[i2]!! * delta_hidden1[j]!!))
                    j = j + 1
                }
                new_w1 = run { val _tmp = new_w1.toMutableList(); _tmp.add(w1row); _tmp }
                i2 = i2 + 1
            }
            net.w1 = new_w1
            s = s + 1
        }
        iter = iter + 1
    }
}

fun predict(net: Network, input: MutableList<Double>): Int {
    var out: Double = feedforward(net, input)
    if (out > 0.6) {
        return 1
    }
    return 0
}

fun example(): Int {
    var inputs: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 1.0), mutableListOf(0.0, 1.0, 0.0), mutableListOf(0.0, 1.0, 1.0), mutableListOf(1.0, 0.0, 0.0), mutableListOf(1.0, 0.0, 1.0), mutableListOf(1.0, 1.0, 0.0), mutableListOf(1.0, 1.0, 1.0))
    var outputs: MutableList<Double> = mutableListOf(0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0)
    var net: Network = new_network()
    train(net, inputs, outputs, 10)
    var result: Int = (predict(net, mutableListOf(1.0, 1.0, 1.0))).toInt()
    println(result.toString())
    return result
}

fun user_main(): Unit {
    example()
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
