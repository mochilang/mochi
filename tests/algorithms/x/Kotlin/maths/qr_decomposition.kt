fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class QR(var q: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var r: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>())
var A: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(12.0, 0.0 - 51.0, 4.0), mutableListOf(6.0, 167.0, 0.0 - 68.0), mutableListOf(0.0 - 4.0, 24.0, 0.0 - 41.0))
var result: QR = qr_decomposition(A)
fun sqrt_approx(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun sign(x: Double): Double {
    if (x >= 0.0) {
        return 1.0
    } else {
        return 0.0 - 1.0
    }
}

fun vector_norm(v: MutableList<Double>): Double {
    var sum: Double = 0.0
    var i: Int = (0).toInt()
    while (i < v.size) {
        sum = sum + (v[i]!! * v[i]!!)
        i = i + 1
    }
    var n: Double = sqrt_approx(sum)
    return n
}

fun identity_matrix(n: Int): MutableList<MutableList<Double>> {
    var mat: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < n) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < n) {
            if (i == j) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1.0); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            }
            j = j + 1
        }
        mat = run { val _tmp = mat.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return mat
}

fun copy_matrix(a: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var mat: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < a.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < (a[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((a[i]!!) as MutableList<Double>)[j]!!); _tmp }
            j = j + 1
        }
        mat = run { val _tmp = mat.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return mat
}

fun matmul(a: MutableList<MutableList<Double>>, b: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var m: Int = (a.size).toInt()
    var n: Int = ((a[0]!!).size).toInt()
    var p: Int = ((b[0]!!).size).toInt()
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < m) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < p) {
            var sum: Double = 0.0
            var k: Int = (0).toInt()
            while (k < n) {
                sum = sum + (((a[i]!!) as MutableList<Double>)[k]!! * ((b[k]!!) as MutableList<Double>)[j]!!)
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

fun qr_decomposition(a: MutableList<MutableList<Double>>): QR {
    var m: Int = (a.size).toInt()
    var n: Int = ((a[0]!!).size).toInt()
    var t: Int = (if (m < n) m else n).toInt()
    var q: MutableList<MutableList<Double>> = identity_matrix(m)
    var r: MutableList<MutableList<Double>> = copy_matrix(a)
    var k: Int = (0).toInt()
    while (k < (t - 1)) {
        var x: MutableList<Double> = mutableListOf<Double>()
        var i: Int = (k).toInt()
        while (i < m) {
            x = run { val _tmp = x.toMutableList(); _tmp.add(((r[i]!!) as MutableList<Double>)[k]!!); _tmp }
            i = i + 1
        }
        var e1: MutableList<Double> = mutableListOf<Double>()
        i = 0
        while (i < x.size) {
            if (i == 0) {
                e1 = run { val _tmp = e1.toMutableList(); _tmp.add(1.0); _tmp }
            } else {
                e1 = run { val _tmp = e1.toMutableList(); _tmp.add(0.0); _tmp }
            }
            i = i + 1
        }
        var alpha: Double = vector_norm(x)
        var s: Double = sign(x[0]!!) * alpha
        var v: MutableList<Double> = mutableListOf<Double>()
        i = 0
        while (i < x.size) {
            v = run { val _tmp = v.toMutableList(); _tmp.add(x[i]!! + (s * e1[i]!!)); _tmp }
            i = i + 1
        }
        var vnorm: Double = vector_norm(v)
        i = 0
        while (i < v.size) {
            _listSet(v, i, v[i]!! / vnorm)
            i = i + 1
        }
        var size: Int = (v.size).toInt()
        var qk_small: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
        i = 0
        while (i < size) {
            var row: MutableList<Double> = mutableListOf<Double>()
            var j: Int = (0).toInt()
            while (j < size) {
                var delta: Double = if (i == j) 1.0 else 0.0.toDouble()
                row = run { val _tmp = row.toMutableList(); _tmp.add(delta - ((2.0 * v[i]!!) * v[j]!!)); _tmp }
                j = j + 1
            }
            qk_small = run { val _tmp = qk_small.toMutableList(); _tmp.add(row); _tmp }
            i = i + 1
        }
        var qk: MutableList<MutableList<Double>> = identity_matrix(m)
        i = 0
        while (i < size) {
            var j: Int = (0).toInt()
            while (j < size) {
                _listSet(qk[k + i]!!, k + j, ((qk_small[i]!!) as MutableList<Double>)[j]!!)
                j = j + 1
            }
            i = i + 1
        }
        q = matmul(q, qk)
        r = matmul(qk, r)
        k = k + 1
    }
    return QR(q = q, r = r)
}

fun print_matrix(mat: MutableList<MutableList<Double>>): Unit {
    var i: Int = (0).toInt()
    while (i < mat.size) {
        var line: String = ""
        var j: Int = (0).toInt()
        while (j < (mat[i]!!).size) {
            line = line + _numToStr(((mat[i]!!) as MutableList<Double>)[j]!!)
            if ((j + 1) < (mat[i]!!).size) {
                line = line + " "
            }
            j = j + 1
        }
        println(line)
        i = i + 1
    }
}

fun main() {
    print_matrix(result.q)
    print_matrix(result.r)
}
