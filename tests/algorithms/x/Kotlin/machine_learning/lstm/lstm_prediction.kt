fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

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

data class LSTMWeights(var w_i: Double = 0.0, var u_i: Double = 0.0, var b_i: Double = 0.0, var w_f: Double = 0.0, var u_f: Double = 0.0, var b_f: Double = 0.0, var w_o: Double = 0.0, var u_o: Double = 0.0, var b_o: Double = 0.0, var w_c: Double = 0.0, var u_c: Double = 0.0, var b_c: Double = 0.0, var w_y: Double = 0.0, var b_y: Double = 0.0)
data class LSTMState(var i: MutableList<Double> = mutableListOf<Double>(), var f: MutableList<Double> = mutableListOf<Double>(), var o: MutableList<Double> = mutableListOf<Double>(), var g: MutableList<Double> = mutableListOf<Double>(), var c: MutableList<Double> = mutableListOf<Double>(), var h: MutableList<Double> = mutableListOf<Double>())
data class Samples(var x: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var y: MutableList<Double> = mutableListOf<Double>())
var data: MutableList<Double> = mutableListOf(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
var look_back: Int = (3).toInt()
var epochs: Int = (200).toInt()
var lr: Double = 0.1
var w: LSTMWeights = train(data, look_back, epochs, lr)
var test_seq: MutableList<Double> = mutableListOf(0.6, 0.7, 0.8)
var pred: Double = predict(test_seq, w)
fun exp_approx(x: Double): Double {
    var sum: Double = 1.0
    var term: Double = 1.0
    var n: Int = (1).toInt()
    while (n < 20) {
        term = (term * x) / ((n.toDouble()))
        sum = sum + term
        n = n + 1
    }
    return sum
}

fun sigmoid(x: Double): Double {
    return 1.0 / (1.0 + exp_approx(0.0 - x))
}

fun tanh_approx(x: Double): Double {
    var e: Double = exp_approx(2.0 * x)
    return (e - 1.0) / (e + 1.0)
}

fun forward(seq: MutableList<Double>, w: LSTMWeights): LSTMState {
    var i_arr: MutableList<Double> = mutableListOf<Double>()
    var f_arr: MutableList<Double> = mutableListOf<Double>()
    var o_arr: MutableList<Double> = mutableListOf<Double>()
    var g_arr: MutableList<Double> = mutableListOf<Double>()
    var c_arr: MutableList<Double> = mutableListOf(0.0)
    var h_arr: MutableList<Double> = mutableListOf(0.0)
    var t: Int = (0).toInt()
    while (t < seq.size) {
        var x: Double = seq[t]!!
        var h_prev: Double = h_arr[t]!!
        var c_prev: Double = c_arr[t]!!
        var i_t: Double = sigmoid(((w.w_i * x) + (w.u_i * h_prev)) + w.b_i)
        var f_t: Double = sigmoid(((w.w_f * x) + (w.u_f * h_prev)) + w.b_f)
        var o_t: Double = sigmoid(((w.w_o * x) + (w.u_o * h_prev)) + w.b_o)
        var g_t: Double = tanh_approx(((w.w_c * x) + (w.u_c * h_prev)) + w.b_c)
        var c_t: Double = (f_t * c_prev) + (i_t * g_t)
        var h_t: Double = o_t * tanh_approx(c_t)
        i_arr = run { val _tmp = i_arr.toMutableList(); _tmp.add(i_t); _tmp }
        f_arr = run { val _tmp = f_arr.toMutableList(); _tmp.add(f_t); _tmp }
        o_arr = run { val _tmp = o_arr.toMutableList(); _tmp.add(o_t); _tmp }
        g_arr = run { val _tmp = g_arr.toMutableList(); _tmp.add(g_t); _tmp }
        c_arr = run { val _tmp = c_arr.toMutableList(); _tmp.add(c_t); _tmp }
        h_arr = run { val _tmp = h_arr.toMutableList(); _tmp.add(h_t); _tmp }
        t = t + 1
    }
    return LSTMState(i = i_arr, f = f_arr, o = o_arr, g = g_arr, c = c_arr, h = h_arr)
}

fun backward(seq: MutableList<Double>, target: Double, w: LSTMWeights, s: LSTMState, lr: Double): LSTMWeights {
    var dw_i: Double = 0.0
    var du_i: Double = 0.0
    var db_i: Double = 0.0
    var dw_f: Double = 0.0
    var du_f: Double = 0.0
    var db_f: Double = 0.0
    var dw_o: Double = 0.0
    var du_o: Double = 0.0
    var db_o: Double = 0.0
    var dw_c: Double = 0.0
    var du_c: Double = 0.0
    var db_c: Double = 0.0
    var dw_y: Double = 0.0
    var db_y: Double = 0.0
    var T: Int = (seq.size).toInt()
    var h_last: Double = (s.h)[T]!!
    var y: Double = (w.w_y * h_last) + w.b_y
    var dy: Double = y - target
    dw_y = dy * h_last
    db_y = dy
    var dh_next: Double = dy * w.w_y
    var dc_next: Double = 0.0
    var t: Int = (T - 1).toInt()
    while (t >= 0) {
        var i_t: Double = (s.i)[t]!!
        var f_t: Double = (s.f)[t]!!
        var o_t: Double = (s.o)[t]!!
        var g_t: Double = (s.g)[t]!!
        var c_t: Double = (s.c)[t + 1]!!
        var c_prev: Double = (s.c)[t]!!
        var h_prev: Double = (s.h)[t]!!
        var tanh_c: Double = tanh_approx(c_t)
        var do_t: Double = dh_next * tanh_c
        var da_o: Double = (do_t * o_t) * (1.0 - o_t)
        var dc: Double = ((dh_next * o_t) * (1.0 - (tanh_c * tanh_c))) + dc_next
        var di_t: Double = dc * g_t
        var da_i: Double = (di_t * i_t) * (1.0 - i_t)
        var dg_t: Double = dc * i_t
        var da_g: Double = dg_t * (1.0 - (g_t * g_t))
        var df_t: Double = dc * c_prev
        var da_f: Double = (df_t * f_t) * (1.0 - f_t)
        dw_i = dw_i + (da_i * seq[t]!!)
        du_i = du_i + (da_i * h_prev)
        db_i = db_i + da_i
        dw_f = dw_f + (da_f * seq[t]!!)
        du_f = du_f + (da_f * h_prev)
        db_f = db_f + da_f
        dw_o = dw_o + (da_o * seq[t]!!)
        du_o = du_o + (da_o * h_prev)
        db_o = db_o + da_o
        dw_c = dw_c + (da_g * seq[t]!!)
        du_c = du_c + (da_g * h_prev)
        db_c = db_c + da_g
        dh_next = (((da_i * w.u_i) + (da_f * w.u_f)) + (da_o * w.u_o)) + (da_g * w.u_c)
        dc_next = dc * f_t
        t = t - 1
    }
    w.w_y = w.w_y - (lr * dw_y)
    w.b_y = w.b_y - (lr * db_y)
    w.w_i = w.w_i - (lr * dw_i)
    w.u_i = w.u_i - (lr * du_i)
    w.b_i = w.b_i - (lr * db_i)
    w.w_f = w.w_f - (lr * dw_f)
    w.u_f = w.u_f - (lr * du_f)
    w.b_f = w.b_f - (lr * db_f)
    w.w_o = w.w_o - (lr * dw_o)
    w.u_o = w.u_o - (lr * du_o)
    w.b_o = w.b_o - (lr * db_o)
    w.w_c = w.w_c - (lr * dw_c)
    w.u_c = w.u_c - (lr * du_c)
    w.b_c = w.b_c - (lr * db_c)
    return w
}

fun make_samples(data: MutableList<Double>, look_back: Int): Samples {
    var X: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var Y: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while ((i + look_back) < data.size) {
        var seq: MutableList<Double> = _sliceList(data, i, i + look_back)
        X = run { val _tmp = X.toMutableList(); _tmp.add((seq as MutableList<Double>)); _tmp }
        Y = run { val _tmp = Y.toMutableList(); _tmp.add(data[i + look_back]!!); _tmp }
        i = i + 1
    }
    return Samples(x = X, y = Y)
}

fun init_weights(): LSTMWeights {
    return LSTMWeights(w_i = 0.1, u_i = 0.2, b_i = 0.0, w_f = 0.1, u_f = 0.2, b_f = 0.0, w_o = 0.1, u_o = 0.2, b_o = 0.0, w_c = 0.1, u_c = 0.2, b_c = 0.0, w_y = 0.1, b_y = 0.0)
}

fun train(data: MutableList<Double>, look_back: Int, epochs: Int, lr: Double): LSTMWeights {
    var samples: Samples = make_samples(data, look_back)
    var w: LSTMWeights = init_weights()
    var ep: Int = (0).toInt()
    while (ep < epochs) {
        var j: Int = (0).toInt()
        while (j < (samples.x).size) {
            var seq: MutableList<Double> = (samples.x)[j]!!
            var target: Double = (samples.y)[j]!!
            var state: LSTMState = forward(seq, w)
            w = backward(seq, target, w, state, lr)
            j = j + 1
        }
        ep = ep + 1
    }
    return w
}

fun predict(seq: MutableList<Double>, w: LSTMWeights): Double {
    var state: LSTMState = forward(seq, w)
    var h_last: Double = (state.h)[(state.h).size - 1]!!
    return (w.w_y * h_last) + w.b_y
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("Predicted value: " + pred.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
