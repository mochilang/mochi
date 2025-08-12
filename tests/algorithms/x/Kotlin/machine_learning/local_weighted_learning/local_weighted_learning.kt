fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun json(v: Any?) { println(toJson(v)) }

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
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

var x_train: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(16.99, 10.34), mutableListOf(21.01, 23.68), mutableListOf(24.59, 25.69))
var y_train: MutableList<Double> = mutableListOf(1.01, 1.66, 3.5)
var preds: MutableList<Double> = local_weight_regression(x_train, y_train, 0.6)
fun expApprox(x: Double): Double {
    if (x < 0.0) {
        return 1.0 / expApprox(0.0 - x)
    }
    if (x > 1.0) {
        var half: Double = expApprox(x / 2.0)
        return half * half
    }
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

fun transpose(mat: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var rows: Int = (mat.size).toInt()
    var cols: Int = ((mat[0]!!).size).toInt()
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < cols) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < rows) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((mat[j]!!) as MutableList<Double>))[i]!!); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun matMul(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var a_rows: Int = (a.size).toInt()
    var a_cols: Int = ((a[0]!!).size).toInt()
    var b_cols: Int = ((b[0]!!).size).toInt()
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a_rows) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < b_cols) {
            var sum: Double = 0.0
            var k: Int = (0).toInt()
            while (k < a_cols) {
                sum = sum + ((((a[i]!!) as MutableList<Double>))[k]!! * (((b[k]!!) as MutableList<Double>))[j]!!)
                k = k + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun matInv(mat: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var n: Int = (mat.size).toInt()
    var aug: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((mat[i]!!) as MutableList<Double>))[j]!!); _tmp }
            j = j + 1
        }
        j = 0
        while (j < n) {
            if (i == j) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1.0); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            }
            j = j + 1
        }
        aug = run { val _tmp = aug.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var col: Int = (0).toInt()
    while (col < n) {
        var pivot: Double = (((aug[col]!!) as MutableList<Double>))[col]!!
        if (pivot == 0.0) {
            panic("Matrix is singular")
        }
        var j: Int = (0).toInt()
        while (j < (2 * n)) {
            _listSet(aug[col]!!, j, (((aug[col]!!) as MutableList<Double>))[j]!! / pivot)
            j = j + 1
        }
        var r: Int = (0).toInt()
        while (r < n) {
            if (r != col) {
                var factor: Double = (((aug[r]!!) as MutableList<Double>))[col]!!
                j = 0
                while (j < (2 * n)) {
                    _listSet(aug[r]!!, j, (((aug[r]!!) as MutableList<Double>))[j]!! - (factor * (((aug[col]!!) as MutableList<Double>))[j]!!))
                    j = j + 1
                }
            }
            r = r + 1
        }
        col = col + 1
    }
    var inv: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    i = 0
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((aug[i]!!) as MutableList<Double>))[j + n]!!); _tmp }
            j = j + 1
        }
        inv = run { val _tmp = inv.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return inv
}

fun weight_matrix(point: MutableList<Double>, x_train: MutableList<MutableList<Double>>, tau: Double): MutableList<MutableList<Double>> {
    var m: Int = (x_train.size).toInt()
    var weights: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < m) {
            if (i == j) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1.0); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            }
            j = j + 1
        }
        weights = run { val _tmp = weights.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    var j: Int = (0).toInt()
    while (j < m) {
        var diff_sq: Double = 0.0
        var k: Int = (0).toInt()
        while (k < point.size) {
            var diff: Double = point[k]!! - (((x_train[j]!!) as MutableList<Double>))[k]!!
            diff_sq = diff_sq + (diff * diff)
            k = k + 1
        }
        _listSet(weights[j]!!, j, expApprox((0.0 - diff_sq) / ((2.0 * tau) * tau)))
        j = j + 1
    }
    return weights
}

fun local_weight(point: MutableList<Double>, x_train: MutableList<MutableList<Double>>, y_train: MutableList<Double>, tau: Double): MutableList<MutableList<Double>> {
    var w: MutableList<MutableList<Double>> = weight_matrix(point, x_train, tau)
    var x_t: MutableList<MutableList<Double>> = transpose(x_train)
    var x_t_w: MutableList<MutableList<Double>> = matMul(x_t, w)
    var x_t_w_x: MutableList<MutableList<Double>> = matMul(x_t_w, x_train)
    var inv_part: MutableList<MutableList<Double>> = matInv(x_t_w_x)
    var y_col: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < y_train.size) {
        y_col = run { val _tmp = y_col.toMutableList(); _tmp.add(mutableListOf(y_train[i]!!)); _tmp }
        i = i + 1
    }
    var x_t_w_y: MutableList<MutableList<Double>> = matMul(x_t_w, y_col)
    return matMul(inv_part, x_t_w_y)
}

fun local_weight_regression(x_train: MutableList<MutableList<Double>>, y_train: MutableList<Double>, tau: Double): MutableList<Double> {
    var m: Int = (x_train.size).toInt()
    var preds: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < m) {
        var theta: MutableList<MutableList<Double>> = local_weight(x_train[i]!!, x_train, y_train, tau)
        var weights_vec: MutableList<Double> = mutableListOf<Double>()
        var k: Int = (0).toInt()
        while (k < theta.size) {
            weights_vec = run { val _tmp = weights_vec.toMutableList(); _tmp.add((((theta[k]!!) as MutableList<Double>))[0]!!); _tmp }
            k = k + 1
        }
        var pred: Double = 0.0
        var j: Int = (0).toInt()
        while (j < (x_train[i]!!).size) {
            pred = pred + ((((x_train[i]!!) as MutableList<Double>))[j]!! * weights_vec[j]!!)
            j = j + 1
        }
        preds = run { val _tmp = preds.toMutableList(); _tmp.add(pred); _tmp }
        i = i + 1
    }
    return preds
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        json((preds as Any?))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
