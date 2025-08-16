import java.math.BigInteger

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

data class Layer(var units: Int = 0, var weight: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var bias: MutableList<Double> = mutableListOf<Double>(), var output: MutableList<Double> = mutableListOf<Double>(), var xdata: MutableList<Double> = mutableListOf<Double>(), var learn_rate: Double = 0.0)
data class Data(var x: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var y: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>())
var seed: Int = (1).toInt()
fun rand(): Int {
    seed = (((Math.floorMod((((seed * 1103515245) + 12345).toLong()), (2147483648L).toInt())).toInt())).toInt()
    return seed
}

fun random(): Double {
    return (1.0 * (rand()).toDouble()) / 2147483648.0
}

fun expApprox(x: Double): Double {
    var y: Double = x
    var is_neg: Boolean = false
    if (x < 0.0) {
        is_neg = true
        y = 0.0 - x
    }
    var term: Double = 1.0
    var sum: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 30) {
        term = (term * y) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    if ((is_neg as Boolean)) {
        return 1.0 / sum
    }
    return sum
}

fun sigmoid(z: Double): Double {
    return 1.0 / (1.0 + expApprox(0.0 - z))
}

fun sigmoid_vec(v: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < v.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(sigmoid(v[i]!!)); _tmp }
        i = i + 1
    }
    return res
}

fun sigmoid_derivative(out: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < out.size) {
        var _val: Double = out[i]!!
        res = run { val _tmp = res.toMutableList(); _tmp.add(_val * (1.0 - _val)); _tmp }
        i = i + 1
    }
    return res
}

fun random_vector(n: Int): MutableList<Double> {
    var v: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < n) {
        v = run { val _tmp = v.toMutableList(); _tmp.add(random() - 0.5); _tmp }
        i = i + 1
    }
    return v
}

fun random_matrix(r: Int, c: Int): MutableList<MutableList<Double>> {
    var m: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < r) {
        m = run { val _tmp = m.toMutableList(); _tmp.add(random_vector(c)); _tmp }
        i = i + 1
    }
    return m
}

fun matvec(mat: MutableList<MutableList<Double>>, vec: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < mat.size) {
        var s: Double = 0.0
        var j: Int = (0).toInt()
        while (j < vec.size) {
            s = s + ((((mat[i]!!) as MutableList<Double>))[j]!! * vec[j]!!)
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(s); _tmp }
        i = i + 1
    }
    return res
}

fun matTvec(mat: MutableList<MutableList<Double>>, vec: MutableList<Double>): MutableList<Double> {
    var cols: Int = ((mat[0]!!).size).toInt()
    var res: MutableList<Double> = mutableListOf<Double>()
    var j: Int = (0).toInt()
    while (j < cols) {
        var s: Double = 0.0
        var i: Int = (0).toInt()
        while (i < mat.size) {
            s = s + ((((mat[i]!!) as MutableList<Double>))[j]!! * vec[i]!!)
            i = i + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(s); _tmp }
        j = j + 1
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

fun vec_scalar_mul(v: MutableList<Double>, s: Double): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < v.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(v[i]!! * s); _tmp }
        i = i + 1
    }
    return res
}

fun outer(a: MutableList<Double>, b: MutableList<Double>): MutableList<MutableList<Double>> {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < b.size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(a[i]!! * b[j]!!); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun mat_scalar_mul(mat: MutableList<MutableList<Double>>, s: Double): MutableList<MutableList<Double>> {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < mat.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < (mat[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((mat[i]!!) as MutableList<Double>))[j]!! * s); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun mat_sub(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < (a[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((a[i]!!) as MutableList<Double>))[j]!! - (((b[i]!!) as MutableList<Double>))[j]!!); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun init_layer(units: Int, back_units: Int, lr: Double): Layer {
    return Layer(units = units, weight = random_matrix(units, back_units), bias = random_vector(units), output = mutableListOf<Double>(), xdata = mutableListOf<Double>(), learn_rate = lr)
}

fun forward(layers: MutableList<Layer>, x: MutableList<Double>): MutableList<Layer> {
    var data: MutableList<Double> = x
    var i: Int = (0).toInt()
    while (i < layers.size) {
        var layer: Layer = layers[i]!!
        layer.xdata = data
        if (i == 0) {
            layer.output = data
        } else {
            var z: MutableList<Double> = vec_sub(matvec(layer.weight, data), layer.bias)
            layer.output = sigmoid_vec(z)
            data = layer.output
        }
        _listSet(layers, i, layer)
        i = i + 1
    }
    return layers
}

fun backward(layers: MutableList<Layer>, grad: MutableList<Double>): MutableList<Layer> {
    var g: MutableList<Double> = grad
    var i: Int = (layers.size - 1).toInt()
    while (i > 0) {
        var layer: Layer = layers[i]!!
        var deriv: MutableList<Double> = sigmoid_derivative(layer.output)
        var delta: MutableList<Double> = vec_mul(g, deriv)
        var grad_w: MutableList<MutableList<Double>> = outer(delta, layer.xdata)
        layer.weight = mat_sub(layer.weight, mat_scalar_mul(grad_w, layer.learn_rate))
        layer.bias = vec_sub(layer.bias, vec_scalar_mul(delta, layer.learn_rate))
        g = matTvec(layer.weight, delta)
        _listSet(layers, i, layer)
        i = i - 1
    }
    return layers
}

fun calc_loss(y: MutableList<Double>, yhat: MutableList<Double>): Double {
    var s: Double = 0.0
    var i: Int = (0).toInt()
    while (i < y.size) {
        var d: Double = y[i]!! - yhat[i]!!
        s = s + (d * d)
        i = i + 1
    }
    return s
}

fun calc_gradient(y: MutableList<Double>, yhat: MutableList<Double>): MutableList<Double> {
    var g: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < y.size) {
        g = run { val _tmp = g.toMutableList(); _tmp.add(2.0 * (yhat[i]!! - y[i]!!)); _tmp }
        i = i + 1
    }
    return g
}

fun train(layers: MutableList<Layer>, xdata: MutableList<MutableList<Double>>, ydata: MutableList<MutableList<Double>>, rounds: Int, acc: Double): Double {
    var layers: MutableList<Layer> = layers
    var r: Int = (0).toInt()
    while (r < rounds) {
        var i: Int = (0).toInt()
        while (i < xdata.size) {
            layers = forward(layers, xdata[i]!!)
            var out: MutableList<Double> = layers[layers.size - 1]!!.output
            var grad: MutableList<Double> = calc_gradient(ydata[i]!!, out)
            layers = backward(layers, grad)
            i = i + 1
        }
        r = r + 1
    }
    return 0.0
}

fun create_data(): Data {
    var x: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < 10) {
        x = run { val _tmp = x.toMutableList(); _tmp.add(random_vector(10)); _tmp }
        i = i + 1
    }
    var y: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.8, 0.4), mutableListOf(0.4, 0.3), mutableListOf(0.34, 0.45), mutableListOf(0.67, 0.32), mutableListOf(0.88, 0.67), mutableListOf(0.78, 0.77), mutableListOf(0.55, 0.66), mutableListOf(0.55, 0.43), mutableListOf(0.54, 0.1), mutableListOf(0.1, 0.5))
    return Data(x = x, y = y)
}

fun user_main(): Unit {
    var data: Data = create_data()
    var x: MutableList<MutableList<Double>> = data.x
    var y: MutableList<MutableList<Double>> = data.y
    var layers: MutableList<Layer> = mutableListOf<Layer>()
    layers = run { val _tmp = layers.toMutableList(); _tmp.add(init_layer(10, 0, 0.3)); _tmp }
    layers = run { val _tmp = layers.toMutableList(); _tmp.add(init_layer(20, 10, 0.3)); _tmp }
    layers = run { val _tmp = layers.toMutableList(); _tmp.add(init_layer(30, 20, 0.3)); _tmp }
    layers = run { val _tmp = layers.toMutableList(); _tmp.add(init_layer(2, 30, 0.3)); _tmp }
    var final_mse: Double = train(layers, x, y, 100, 0.01)
    println(final_mse)
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
