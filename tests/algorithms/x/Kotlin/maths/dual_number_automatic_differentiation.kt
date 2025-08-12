import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Dual(var real: Double = 0.0, var duals: MutableList<Double> = mutableListOf<Double>())
fun make_dual(real: Double, rank: Int): Dual {
    var ds: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < rank) {
        ds = run { val _tmp = ds.toMutableList(); _tmp.add(1.0); _tmp }
        i = i + 1
    }
    return Dual(real = real, duals = ds)
}

fun dual_from_list(real: Double, ds: MutableList<Double>): Dual {
    return Dual(real = real, duals = ds)
}

fun dual_add(a: Dual, b: Dual): Dual {
    var s_dual: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < (a.duals).size) {
        s_dual = run { val _tmp = s_dual.toMutableList(); _tmp.add((a.duals)[i]!!); _tmp }
        i = i + 1
    }
    var o_dual: MutableList<Double> = mutableListOf<Double>()
    var j: Int = (0).toInt()
    while (j < (b.duals).size) {
        o_dual = run { val _tmp = o_dual.toMutableList(); _tmp.add((b.duals)[j]!!); _tmp }
        j = j + 1
    }
    if (s_dual.size > o_dual.size) {
        var diff: BigInteger = ((s_dual.size - o_dual.size).toBigInteger())
        var k: Int = (0).toInt()
        while ((k).toBigInteger().compareTo((diff)) < 0) {
            o_dual = run { val _tmp = o_dual.toMutableList(); _tmp.add(1.0); _tmp }
            k = k + 1
        }
    } else {
        if (s_dual.size < o_dual.size) {
            var diff2: BigInteger = ((o_dual.size - s_dual.size).toBigInteger())
            var k2: Int = (0).toInt()
            while ((k2).toBigInteger().compareTo((diff2)) < 0) {
                s_dual = run { val _tmp = s_dual.toMutableList(); _tmp.add(1.0); _tmp }
                k2 = k2 + 1
            }
        }
    }
    var new_duals: MutableList<Double> = mutableListOf<Double>()
    var idx: Int = (0).toInt()
    while (idx < s_dual.size) {
        new_duals = run { val _tmp = new_duals.toMutableList(); _tmp.add(s_dual[idx]!! + o_dual[idx]!!); _tmp }
        idx = idx + 1
    }
    return Dual(real = a.real + b.real, duals = new_duals)
}

fun dual_add_real(a: Dual, b: Double): Dual {
    var ds: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < (a.duals).size) {
        ds = run { val _tmp = ds.toMutableList(); _tmp.add((a.duals)[i]!!); _tmp }
        i = i + 1
    }
    return Dual(real = a.real + b, duals = ds)
}

fun dual_mul(a: Dual, b: Dual): Dual {
    var new_len: Int = (((a.duals).size + (b.duals).size) + 1).toInt()
    var new_duals: MutableList<Double> = mutableListOf<Double>()
    var idx: Int = (0).toInt()
    while (idx < new_len) {
        new_duals = run { val _tmp = new_duals.toMutableList(); _tmp.add(0.0); _tmp }
        idx = idx + 1
    }
    var i: Int = (0).toInt()
    while (i < (a.duals).size) {
        var j: Int = (0).toInt()
        while (j < (b.duals).size) {
            var pos: Int = ((i + j) + 1).toInt()
            var _val: Double = new_duals[pos]!! + ((a.duals)[i]!! * (b.duals)[j]!!)
            _listSet(new_duals, pos, _val)
            j = j + 1
        }
        i = i + 1
    }
    var k: Int = (0).toInt()
    while (k < (a.duals).size) {
        var _val: Double = new_duals[k]!! + ((a.duals)[k]!! * b.real)
        _listSet(new_duals, k, _val)
        k = k + 1
    }
    var l: Int = (0).toInt()
    while (l < (b.duals).size) {
        var _val: Double = new_duals[l]!! + ((b.duals)[l]!! * a.real)
        _listSet(new_duals, l, _val)
        l = l + 1
    }
    return Dual(real = a.real * b.real, duals = new_duals)
}

fun dual_mul_real(a: Dual, b: Double): Dual {
    var ds: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < (a.duals).size) {
        ds = run { val _tmp = ds.toMutableList(); _tmp.add((a.duals)[i]!! * b); _tmp }
        i = i + 1
    }
    return Dual(real = a.real * b, duals = ds)
}

fun dual_pow(x: Dual, n: Int): Dual {
    if (n < 0) {
        panic("power must be a positive integer")
    }
    if (n == 0) {
        return Dual(real = 1.0, duals = mutableListOf<Double>())
    }
    var res: Dual = x
    var i: Int = (1).toInt()
    while (i < n) {
        res = dual_mul(res, x)
        i = i + 1
    }
    return res
}

fun factorial(n: Int): Double {
    var res: Double = 1.0
    var i: Int = (2).toInt()
    while (i <= n) {
        res = res * ((i.toDouble()))
        i = i + 1
    }
    return res
}

fun differentiate(func: (Dual) -> Dual, position: Double, order: Int): Double {
    var d: Dual = make_dual(position, 1)
    var result: Dual = ((func(d)) as Dual)
    if (order == 0) {
        return result.real
    }
    return (result.duals)[order - 1]!! * factorial(order)
}

fun test_differentiate(): Unit {
    fun f1(x: Dual): Dual {
        return dual_pow(x, 2)
    }

    if (differentiate(::f1, 2.0, 2) != 2.0) {
        panic("f1 failed")
    }
    fun f2(x: Dual): Dual {
        return dual_mul(dual_pow(x, 2), dual_pow(x, 4))
    }

    if (differentiate(::f2, 9.0, 2) != 196830.0) {
        panic("f2 failed")
    }
    fun f3(y: Dual): Dual {
        return dual_mul_real(dual_pow(dual_add_real(y, 3.0), 6), 0.5)
    }

    if (differentiate(::f3, 3.5, 4) != 7605.0) {
        panic("f3 failed")
    }
    fun f4(y: Dual): Dual {
        return dual_pow(y, 2)
    }

    if (differentiate(::f4, 4.0, 3) != 0.0) {
        panic("f4 failed")
    }
}

fun user_main(): Unit {
    test_differentiate()
    fun f(y: Dual): Dual {
        return dual_mul(dual_pow(y, 2), dual_pow(y, 4))
    }

    var res: Double = differentiate(::f, 9.0, 2)
    println(res)
}

fun main() {
    user_main()
}
