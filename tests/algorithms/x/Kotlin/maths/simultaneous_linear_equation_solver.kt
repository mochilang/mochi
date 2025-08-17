import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun floor(x: Double): Double {
    var i: Int = (x.toInt()).toInt()
    if ((i.toDouble()) > x) {
        i = i - 1
    }
    return i.toDouble()
}

fun pow10(n: Int): Double {
    var p: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        p = p * 10.0
        i = i + 1
    }
    return p
}

fun round(x: Double, n: Int): Double {
    var m: Double = pow10(n)
    return floor((x * m) + 0.5) / m
}

fun clone_matrix(mat: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var new_mat: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var i: Int = (0).toInt()
    while (i < mat.size) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var j: Int = (0).toInt()
        while (j < (mat[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(((mat[i]!!) as MutableList<Double>)[j]!!); _tmp }
            j = j + 1
        }
        new_mat = run { val _tmp = new_mat.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return new_mat
}

fun solve_simultaneous(equations: MutableList<MutableList<Double>>): MutableList<Double> {
    var n: Int = (equations.size).toInt()
    if (n == 0) {
        panic("solve_simultaneous() requires n lists of length n+1")
    }
    var m: Int = (n + 1).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        if ((equations[i]!!).size != m) {
            panic("solve_simultaneous() requires n lists of length n+1")
        }
        i = i + 1
    }
    var a: MutableList<MutableList<Double>> = clone_matrix(equations)
    var row: Int = (0).toInt()
    while (row < n) {
        var pivot: Int = (row).toInt()
        while ((pivot < n) && (((a[pivot]!!) as MutableList<Double>)[row]!! == 0.0)) {
            pivot = pivot + 1
        }
        if (pivot == n) {
            panic("solve_simultaneous() requires at least 1 full equation")
        }
        if (pivot != row) {
            var temp: MutableList<Double> = a[row]!!
            _listSet(a, row, a[pivot]!!)
            _listSet(a, pivot, temp)
        }
        var pivot_val: Double = ((a[row]!!) as MutableList<Double>)[row]!!
        var col: Int = (0).toInt()
        while (col < m) {
            _listSet(a[row]!!, col, ((a[row]!!) as MutableList<Double>)[col]!! / pivot_val)
            col = col + 1
        }
        var r: Int = (0).toInt()
        while (r < n) {
            if (r != row) {
                var factor: Double = ((a[r]!!) as MutableList<Double>)[row]!!
                var c: Int = (0).toInt()
                while (c < m) {
                    _listSet(a[r]!!, c, ((a[r]!!) as MutableList<Double>)[c]!! - (factor * ((a[row]!!) as MutableList<Double>)[c]!!))
                    c = c + 1
                }
            }
            r = r + 1
        }
        row = row + 1
    }
    var res: MutableList<Double> = mutableListOf<Double>()
    var k: Int = (0).toInt()
    while (k < n) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(round(((a[k]!!) as MutableList<Double>)[m - 1]!!, 5)); _tmp }
        k = k + 1
    }
    return res
}

fun test_solver(): Unit {
    var a: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 2.0, 3.0), mutableListOf(4.0, 5.0, 6.0))
    var r1: MutableList<Double> = solve_simultaneous(a)
    if (!(((((r1.size == 2) && (r1[0]!! == (0.0 - 1.0)) as Boolean)) && (r1[1]!! == 2.0)) as Boolean)) {
        panic("test1 failed")
    }
    var b: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0 - 3.0, 1.0, 7.0), mutableListOf(3.0, 2.0, 0.0 - 1.0, 11.0), mutableListOf(5.0, 1.0, 0.0 - 2.0, 12.0))
    var r2: MutableList<Double> = solve_simultaneous(b)
    if (!(((((((r2.size == 3) && (r2[0]!! == 6.4) as Boolean)) && (r2[1]!! == 1.2) as Boolean)) && (r2[2]!! == 10.6)) as Boolean)) {
        panic("test2 failed")
    }
}

fun user_main(): Unit {
    test_solver()
    var eq: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.0, 1.0, 1.0, 1.0, 1.0, 4.0), mutableListOf(1.0, 2.0, 1.0, 1.0, 1.0, 5.0), mutableListOf(1.0, 1.0, 2.0, 1.0, 1.0, 6.0), mutableListOf(1.0, 1.0, 1.0, 2.0, 1.0, 7.0), mutableListOf(1.0, 1.0, 1.0, 1.0, 2.0, 8.0))
    println(solve_simultaneous(eq).toString())
    println(solve_simultaneous(mutableListOf(mutableListOf(4.0, 2.0))).toString())
}

fun main() {
    user_main()
}
