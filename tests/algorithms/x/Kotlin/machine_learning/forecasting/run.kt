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

fun int_to_float(x: Int): Double {
    return x * 1.0
}

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun exp_approx(x: Double): Double {
    var term: Double = 1.0
    var sum: Double = 1.0
    var i: Int = (1).toInt()
    while (i < 10) {
        term = (term * x) / int_to_float(i)
        sum = sum + term
        i = i + 1
    }
    return sum
}

fun floor_int(x: Double): Int {
    var i: Int = (0).toInt()
    while (int_to_float(i + 1) <= x) {
        i = i + 1
    }
    return i
}

fun dot(a: MutableList<Double>, b: MutableList<Double>): Double {
    var s: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        s = s + (a[i]!! * b[i]!!)
        i = i + 1
    }
    return s
}

fun transpose(m: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows: Int = (m.size).toInt()
    var cols: Int = ((m[0]!!).size).toInt()
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var j: Int = (0).toInt()
    while (j < cols) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (0).toInt()
        while (i < rows) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((m[i]!!) as MutableList<Double>))[j]!!); _tmp }
            i = i + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        j = j + 1
    }
    return res
}

fun matmul(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var n: Int = (a.size).toInt()
    var m: Int = ((b[0]!!).size).toInt()
    var p: Int = (b.size).toInt()
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < m) {
            var s: Double = 0.0
            var k: Int = (0).toInt()
            while (k < p) {
                s = s + ((((a[i]!!) as MutableList<Double>))[k]!! * (((b[k]!!) as MutableList<Double>))[j]!!)
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(s); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun matvec(a: MutableList<MutableList<Double>>, b: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < a.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(dot(a[i]!!, b)); _tmp }
        i = i + 1
    }
    return res
}

fun identity(n: Int): MutableList<MutableList<Double>> {
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((if (i == j) 1.0 else 0.0.toDouble())); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun invert(mat: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var n: Int = (mat.size).toInt()
    var a: MutableList<MutableList<Double>> = mat
    var inv: MutableList<MutableList<Double>> = identity(n)
    var i: Int = (0).toInt()
    while (i < n) {
        var pivot: Double = (((a[i]!!) as MutableList<Double>))[i]!!
        var j: Int = (0).toInt()
        while (j < n) {
            _listSet(a[i]!!, j, (((a[i]!!) as MutableList<Double>))[j]!! / pivot)
            _listSet(inv[i]!!, j, (((inv[i]!!) as MutableList<Double>))[j]!! / pivot)
            j = j + 1
        }
        var k: Int = (0).toInt()
        while (k < n) {
            if (k != i) {
                var factor: Double = (((a[k]!!) as MutableList<Double>))[i]!!
                j = 0
                while (j < n) {
                    _listSet(a[k]!!, j, (((a[k]!!) as MutableList<Double>))[j]!! - (factor * (((a[i]!!) as MutableList<Double>))[j]!!))
                    _listSet(inv[k]!!, j, (((inv[k]!!) as MutableList<Double>))[j]!! - (factor * (((inv[i]!!) as MutableList<Double>))[j]!!))
                    j = j + 1
                }
            }
            k = k + 1
        }
        i = i + 1
    }
    return inv
}

fun normal_equation(X: MutableList<MutableList<Double>>, y: MutableList<Double>): MutableList<Double> {
    var Xt: MutableList<MutableList<Double>> = transpose(X)
    var XtX: MutableList<MutableList<Double>> = matmul(Xt, X)
    var XtX_inv: MutableList<MutableList<Double>> = invert(XtX)
    var Xty: MutableList<Double> = matvec(Xt, y)
    return matvec(XtX_inv, Xty)
}

fun linear_regression_prediction(train_dt: MutableList<Double>, train_usr: MutableList<Double>, train_mtch: MutableList<Double>, test_dt: MutableList<Double>, test_mtch: MutableList<Double>): Double {
    var X: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < train_dt.size) {
        X = run { val _tmp = X.toMutableList(); _tmp.add(mutableListOf(1.0, train_dt[i]!!, train_mtch[i]!!)); _tmp }
        i = i + 1
    }
    var beta: MutableList<Double> = normal_equation(X, train_usr)
    return abs_float((beta[0]!! + (test_dt[0]!! * beta[1]!!)) + (test_mtch[0]!! * beta[2]!!))
}

fun sarimax_predictor(train_user: MutableList<Double>, train_match: MutableList<Double>, test_match: MutableList<Double>): Double {
    var n: Int = (train_user.size).toInt()
    var X: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var y: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (1).toInt()
    while (i < n) {
        X = run { val _tmp = X.toMutableList(); _tmp.add(mutableListOf(1.0, train_user[i - 1]!!, train_match[i]!!)); _tmp }
        y = run { val _tmp = y.toMutableList(); _tmp.add(train_user[i]!!); _tmp }
        i = i + 1
    }
    var beta: MutableList<Double> = normal_equation(X, y)
    return (beta[0]!! + (beta[1]!! * train_user[n - 1]!!)) + (beta[2]!! * test_match[0]!!)
}

fun rbf_kernel(a: MutableList<Double>, b: MutableList<Double>, gamma: Double): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < a.size) {
        var diff: Double = a[i]!! - b[i]!!
        sum = sum + (diff * diff)
        i = i + 1
    }
    return exp_approx((0.0 - gamma) * sum)
}

fun support_vector_regressor(x_train: MutableList<MutableList<Double>>, x_test: MutableList<MutableList<Double>>, train_user: MutableList<Double>): Double {
    var gamma: Double = 0.1
    var weights: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < x_train.size) {
        weights = run { val _tmp = weights.toMutableList(); _tmp.add(rbf_kernel(x_train[i]!!, x_test[0]!!, gamma)); _tmp }
        i = i + 1
    }
    var num: Double = 0.0
    var den: Double = 0.0
    i = 0
    while (i < train_user.size) {
        num = num + (weights[i]!! * train_user[i]!!)
        den = den + weights[i]!!
        i = i + 1
    }
    return num / den
}

fun set_at_float(xs: MutableList<Double>, idx: Int, value: Double): MutableList<Double> {
    var i: Int = (0).toInt()
    var res: MutableList<Double> = mutableListOf<Double>()
    while (i < xs.size) {
        if (i == idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun sort_float(xs: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = xs
    var i: Int = (1).toInt()
    while (i < res.size) {
        var key: Double = res[i]!!
        var j: BigInteger = ((i - 1).toBigInteger())
        while ((j.compareTo((0).toBigInteger()) >= 0) && (res[(j).toInt()]!! > key)) {
            res = set_at_float(res, ((j.add((1).toBigInteger())).toInt()), res[(j).toInt()]!!)
            j = j.subtract((1).toBigInteger())
        }
        res = set_at_float(res, ((j.add((1).toBigInteger())).toInt()), key)
        i = i + 1
    }
    return res
}

fun percentile(data: MutableList<Double>, q: Double): Double {
    var sorted: MutableList<Double> = sort_float(data)
    var n: Int = (sorted.size).toInt()
    var pos: Double = (q / 100.0) * int_to_float(n - 1)
    var idx: Int = (floor_int(pos)).toInt()
    var frac: Double = pos - int_to_float(idx)
    if ((idx + 1) < n) {
        return (sorted[idx]!! * (1.0 - frac)) + (sorted[idx + 1]!! * frac)
    }
    return sorted[idx]!!
}

fun interquartile_range_checker(train_user: MutableList<Double>): Double {
    var q1: Double = percentile(train_user, 25.0)
    var q3: Double = percentile(train_user, 75.0)
    var iqr: Double = q3 - q1
    return q1 - (iqr * 0.1)
}

fun data_safety_checker(list_vote: MutableList<Double>, actual_result: Double): Boolean {
    var safe: Int = (0).toInt()
    var not_safe: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < list_vote.size) {
        var v: Double = list_vote[i]!!
        if (v > actual_result) {
            safe = not_safe + 1
        } else {
            if (abs_float(abs_float(v) - abs_float(actual_result)) <= 0.1) {
                safe = safe + 1
            } else {
                not_safe = not_safe + 1
            }
        }
        i = i + 1
    }
    return safe > not_safe
}

fun user_main(): Unit {
    var vote: MutableList<Double> = mutableListOf(linear_regression_prediction(mutableListOf(2.0, 3.0, 4.0, 5.0), mutableListOf(5.0, 3.0, 4.0, 6.0), mutableListOf(3.0, 1.0, 2.0, 4.0), mutableListOf(2.0), mutableListOf(2.0)), sarimax_predictor(mutableListOf(4.0, 2.0, 6.0, 8.0), mutableListOf(3.0, 1.0, 2.0, 4.0), mutableListOf(2.0)), support_vector_regressor(mutableListOf(mutableListOf(5.0, 2.0), mutableListOf(1.0, 5.0), mutableListOf(6.0, 2.0)), mutableListOf(mutableListOf(3.0, 2.0)), mutableListOf(2.0, 1.0, 4.0)))
    println(vote[0]!!)
    println(vote[1]!!)
    println(vote[2]!!)
    println(data_safety_checker(vote, 5.0))
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
