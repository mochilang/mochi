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

data class CNN(var conv_kernels: MutableList<MutableList<MutableList<Double>>> = mutableListOf<MutableList<MutableList<Double>>>(), var conv_bias: MutableList<Double> = mutableListOf<Double>(), var conv_step: Int = 0, var pool_size: Int = 0, var w_hidden: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var w_out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var b_hidden: MutableList<Double> = mutableListOf<Double>(), var b_out: MutableList<Double> = mutableListOf<Double>(), var rate_weight: Double = 0.0, var rate_bias: Double = 0.0)
data class TrainSample(var image: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var target: MutableList<Double> = mutableListOf<Double>())
var seed: Int = (1).toInt()
fun random(): Double {
    seed = (Math.floorMod(((seed * 13) + 7), 100)).toInt()
    return (seed.toDouble()) / 100.0
}

fun sigmoid(x: Double): Double {
    return 1.0 / (1.0 + kotlin.math.exp(0.0 - x))
}

fun to_float(x: Int): Double {
    return (x).toDouble() * 1.0
}

fun exp(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / to_float(n)
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun convolve(data: MutableList<MutableList<Double>>, kernel: MutableList<MutableList<Double>>, step: Int, bias: Double): MutableList<MutableList<Double>> {
    var size_data: Int = (data.size).toInt()
    var size_kernel: Int = (kernel.size).toInt()
    var out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i <= (size_data - size_kernel)) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j <= (size_data - size_kernel)) {
            var sum: Double = 0.0
            var a: Int = (0).toInt()
            while (a < size_kernel) {
                var b: Int = (0).toInt()
                while (b < size_kernel) {
                    sum = sum + (((data[i + a]!!) as MutableList<Double>)[j + b]!! * ((kernel[a]!!) as MutableList<Double>)[b]!!)
                    b = b + 1
                }
                a = a + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sigmoid(sum - bias)); _tmp }
            j = j + step
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(row); _tmp }
        i = i + step
    }
    return out
}

fun average_pool(map: MutableList<MutableList<Double>>, size: Int): MutableList<MutableList<Double>> {
    var out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < map.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < (map[i]!!).size) {
            var sum: Double = 0.0
            var a: Int = (0).toInt()
            while (a < size) {
                var b: Int = (0).toInt()
                while (b < size) {
                    sum = sum + ((map[i + a]!!) as MutableList<Double>)[j + b]!!
                    b = b + 1
                }
                a = a + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum / ((size * size).toDouble())); _tmp }
            j = j + size
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(row); _tmp }
        i = i + size
    }
    return out
}

fun flatten(maps: MutableList<MutableList<MutableList<Double>>>): MutableList<Double> {
    var out: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < maps.size) {
        var j: Int = (0).toInt()
        while (j < (maps[i]!!).size) {
            var k: Int = (0).toInt()
            while (k < (((maps[i]!!) as MutableList<MutableList<Double>>)[j]!!).size) {
                out = run { val _tmp = out.toMutableList(); _tmp.add(((((maps[i]!!) as MutableList<MutableList<Double>>)[j]!!) as MutableList<Double>)[k]!!); _tmp }
                k = k + 1
            }
            j = j + 1
        }
        i = i + 1
    }
    return out
}

fun vec_mul_mat(v: MutableList<Double>, m: MutableList<MutableList<Double>>): MutableList<Double> {
    var cols: Int = ((m[0]!!).size).toInt()
    var res: MutableList<Double> = mutableListOf<Double>()
    var j: Int = (0).toInt()
    while (j < cols) {
        var sum: Double = 0.0
        var i: Int = (0).toInt()
        while (i < v.size) {
            sum = sum + (v[i]!! * ((m[i]!!) as MutableList<Double>)[j]!!)
            i = i + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(sum); _tmp }
        j = j + 1
    }
    return res
}

fun matT_vec_mul(m: MutableList<MutableList<Double>>, v: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < m.size) {
        var sum: Double = 0.0
        var j: Int = (0).toInt()
        while (j < (m[i]!!).size) {
            sum = sum + (((m[i]!!) as MutableList<Double>)[j]!! * v[j]!!)
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(sum); _tmp }
        i = i + 1
    }
    return res
}

fun vec_add(a: MutableList<Double>, b: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < a.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(a[i]!! + b[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun vec_sub(a: MutableList<Double>, b: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < a.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(a[i]!! - b[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun vec_mul(a: MutableList<Double>, b: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < a.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(a[i]!! * b[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun vec_map_sig(v: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < v.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(sigmoid(v[i]!!)); _tmp }
        i = i + 1
    }
    return res
}

fun new_cnn(): CNN {
    var k1: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 0.0), mutableListOf(0.0, 1.0))
    var k2: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 1.0), mutableListOf(1.0, 0.0))
    var conv_kernels: MutableList<MutableList<MutableList<Double>>> = mutableListOf(k1, k2)
    var conv_bias: MutableList<Double> = mutableListOf(0.0, 0.0)
    var conv_step: Int = (2).toInt()
    var pool_size: Int = (2).toInt()
    var input_size: Int = (2).toInt()
    var hidden_size: Int = (2).toInt()
    var output_size: Int = (2).toInt()
    var w_hidden: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < input_size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < hidden_size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(random() - 0.5); _tmp }
            j = j + 1
        }
        w_hidden = run { val _tmp = w_hidden.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var w_out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    i = 0
    while (i < hidden_size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < output_size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(random() - 0.5); _tmp }
            j = j + 1
        }
        w_out = run { val _tmp = w_out.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var b_hidden: MutableList<Double> = mutableListOf(0.0, 0.0)
    var b_out: MutableList<Double> = mutableListOf(0.0, 0.0)
    return CNN(conv_kernels = conv_kernels, conv_bias = conv_bias, conv_step = conv_step, pool_size = pool_size, w_hidden = w_hidden, w_out = w_out, b_hidden = b_hidden, b_out = b_out, rate_weight = 0.2, rate_bias = 0.2)
}

fun forward(cnn: CNN, data: MutableList<MutableList<Double>>): MutableList<Double> {
    var maps: MutableList<MutableList<MutableList<Double>>> = mutableListOf<MutableList<MutableList<Double>>>()
    var i: Int = (0).toInt()
    while (i < (cnn.conv_kernels).size) {
        var conv_map: MutableList<MutableList<Double>> = convolve(data, (cnn.conv_kernels)[i]!!, cnn.conv_step, (cnn.conv_bias)[i]!!)
        var pooled: MutableList<MutableList<Double>> = average_pool(conv_map, cnn.pool_size)
        maps = run { val _tmp = maps.toMutableList(); _tmp.add(pooled); _tmp }
        i = i + 1
    }
    var flat: MutableList<Double> = flatten(maps)
    var hidden_net: MutableList<Double> = vec_add(vec_mul_mat(flat, cnn.w_hidden), cnn.b_hidden)
    var hidden_out: MutableList<Double> = vec_map_sig(hidden_net)
    var out_net: MutableList<Double> = vec_add(vec_mul_mat(hidden_out, cnn.w_out), cnn.b_out)
    var out: MutableList<Double> = vec_map_sig(out_net)
    return out
}

fun train(cnn: CNN, samples: MutableList<TrainSample>, epochs: Int): CNN {
    var w_out: MutableList<MutableList<Double>> = cnn.w_out
    var b_out: MutableList<Double> = cnn.b_out
    var w_hidden: MutableList<MutableList<Double>> = cnn.w_hidden
    var b_hidden: MutableList<Double> = cnn.b_hidden
    var e: Int = (0).toInt()
    while (e < epochs) {
        var s: Int = (0).toInt()
        while (s < samples.size) {
            var data: MutableList<MutableList<Double>> = samples[s]!!.image
            var target: MutableList<Double> = samples[s]!!.target
            var maps: MutableList<MutableList<MutableList<Double>>> = mutableListOf<MutableList<MutableList<Double>>>()
            var i: Int = (0).toInt()
            while (i < (cnn.conv_kernels).size) {
                var conv_map: MutableList<MutableList<Double>> = convolve(data, (cnn.conv_kernels)[i]!!, cnn.conv_step, (cnn.conv_bias)[i]!!)
                var pooled: MutableList<MutableList<Double>> = average_pool(conv_map, cnn.pool_size)
                maps = run { val _tmp = maps.toMutableList(); _tmp.add(pooled); _tmp }
                i = i + 1
            }
            var flat: MutableList<Double> = flatten(maps)
            var hidden_net: MutableList<Double> = vec_add(vec_mul_mat(flat, w_hidden), b_hidden)
            var hidden_out: MutableList<Double> = vec_map_sig(hidden_net)
            var out_net: MutableList<Double> = vec_add(vec_mul_mat(hidden_out, w_out), b_out)
            var out: MutableList<Double> = vec_map_sig(out_net)
            var error_out: MutableList<Double> = vec_sub(target, out)
            var pd_out: MutableList<Double> = vec_mul(error_out, vec_mul(out, vec_sub(mutableListOf(1.0, 1.0), out)))
            var error_hidden: MutableList<Double> = matT_vec_mul(w_out, pd_out)
            var pd_hidden: MutableList<Double> = vec_mul(error_hidden, vec_mul(hidden_out, vec_sub(mutableListOf(1.0, 1.0), hidden_out)))
            var j: Int = (0).toInt()
            while (j < w_out.size) {
                var k: Int = (0).toInt()
                while (k < (w_out[j]!!).size) {
                    _listSet(w_out[j]!!, k, ((w_out[j]!!) as MutableList<Double>)[k]!! + ((cnn.rate_weight * hidden_out[j]!!) * pd_out[k]!!))
                    k = k + 1
                }
                j = j + 1
            }
            j = 0
            while (j < b_out.size) {
                _listSet(b_out, j, b_out[j]!! - (cnn.rate_bias * pd_out[j]!!))
                j = j + 1
            }
            var i_h: Int = (0).toInt()
            while (i_h < w_hidden.size) {
                var j_h: Int = (0).toInt()
                while (j_h < (w_hidden[i_h]!!).size) {
                    _listSet(w_hidden[i_h]!!, j_h, ((w_hidden[i_h]!!) as MutableList<Double>)[j_h]!! + ((cnn.rate_weight * flat[i_h]!!) * pd_hidden[j_h]!!))
                    j_h = j_h + 1
                }
                i_h = i_h + 1
            }
            j = 0
            while (j < b_hidden.size) {
                _listSet(b_hidden, j, b_hidden[j]!! - (cnn.rate_bias * pd_hidden[j]!!))
                j = j + 1
            }
            s = s + 1
        }
        e = e + 1
    }
    return CNN(conv_kernels = cnn.conv_kernels, conv_bias = cnn.conv_bias, conv_step = cnn.conv_step, pool_size = cnn.pool_size, w_hidden = w_hidden, w_out = w_out, b_hidden = b_hidden, b_out = b_out, rate_weight = cnn.rate_weight, rate_bias = cnn.rate_bias)
}

fun user_main(): Unit {
    var cnn: CNN = new_cnn()
    var image: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 0.0, 1.0, 0.0), mutableListOf(0.0, 1.0, 0.0, 1.0), mutableListOf(1.0, 0.0, 1.0, 0.0), mutableListOf(0.0, 1.0, 0.0, 1.0))
    var sample: TrainSample = TrainSample(image = image, target = mutableListOf(1.0, 0.0))
    println(listOf("Before training:", forward(cnn, image)).joinToString(" "))
    var trained: CNN = train(cnn, mutableListOf(sample), 50)
    println(listOf("After training:", forward(trained, image)).joinToString(" "))
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
