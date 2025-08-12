import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var fib_cache_global: MutableMap<Int, Int> = mutableMapOf<Int, Int>()
var fib_memo_cache: MutableMap<Int, Int> = (mutableMapOf<Int, Int>(0 to (0), 1 to (1), 2 to (1)) as MutableMap<Int, Int>)
fun sqrt(x: Double): Double {
    if (x <= 0.0) {
        return 0.0
    }
    var guess: Double = x
    var i: Int = (0).toInt()
    while (i < 10) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun powf(x: Double, n: Int): Double {
    var res: Double = 1.0
    var i: Int = (0).toInt()
    while (i < n) {
        res = res * x
        i = i + 1
    }
    return res
}

fun roundf(x: Double): Int {
    if (x >= 0.0) {
        return ((x + 0.5).toInt())
    }
    return ((x - 0.5).toInt())
}

fun fib_iterative(n: Int): MutableList<Int> {
    if (n < 0) {
        panic("n is negative")
    }
    if (n == 0) {
        return mutableListOf(0)
    }
    var fib: MutableList<Int> = mutableListOf(0, 1)
    var i: Int = (2).toInt()
    while (i <= n) {
        fib = run { val _tmp = fib.toMutableList(); _tmp.add(fib[i - 1]!! + fib[i - 2]!!); _tmp }
        i = i + 1
    }
    return fib
}

fun fib_recursive_term(i: Int): Int {
    if (i < 0) {
        panic("n is negative")
    }
    if (i < 2) {
        return i
    }
    return fib_recursive_term(i - 1) + fib_recursive_term(i - 2)
}

fun fib_recursive(n: Int): MutableList<Int> {
    if (n < 0) {
        panic("n is negative")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= n) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(fib_recursive_term(i)); _tmp }
        i = i + 1
    }
    return res
}

fun fib_recursive_cached_term(i: Int): Int {
    if (i < 0) {
        panic("n is negative")
    }
    if (i < 2) {
        return i
    }
    if (i in fib_cache_global) {
        return (fib_cache_global)[i] as Int
    }
    var _val: Int = (fib_recursive_cached_term(i - 1) + fib_recursive_cached_term(i - 2)).toInt()
    (fib_cache_global)[i] = _val
    return _val
}

fun fib_recursive_cached(n: Int): MutableList<Int> {
    if (n < 0) {
        panic("n is negative")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var j: Int = (0).toInt()
    while (j <= n) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(fib_recursive_cached_term(j)); _tmp }
        j = j + 1
    }
    return res
}

fun fib_memoization_term(num: Int): Int {
    if (num in fib_memo_cache) {
        return (fib_memo_cache)[num] as Int
    }
    var value: Int = (fib_memoization_term(num - 1) + fib_memoization_term(num - 2)).toInt()
    (fib_memo_cache)[num] = value
    return value
}

fun fib_memoization(n: Int): MutableList<Int> {
    if (n < 0) {
        panic("n is negative")
    }
    var out: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= n) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(fib_memoization_term(i)); _tmp }
        i = i + 1
    }
    return out
}

fun fib_binet(n: Int): MutableList<Int> {
    if (n < 0) {
        panic("n is negative")
    }
    if (n >= 1475) {
        panic("n is too large")
    }
    var sqrt5: Double = sqrt(5.0)
    var phi: Double = (1.0 + sqrt5) / 2.0
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= n) {
        var _val: Int = (roundf(powf(phi, i) / sqrt5)).toInt()
        res = run { val _tmp = res.toMutableList(); _tmp.add(_val); _tmp }
        i = i + 1
    }
    return res
}

fun matrix_mul(a: MutableList<MutableList<Int>>, b: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var a00: Int = (((((a[0]!!) as MutableList<Int>))[0]!! * (((b[0]!!) as MutableList<Int>))[0]!!) + ((((a[0]!!) as MutableList<Int>))[1]!! * (((b[1]!!) as MutableList<Int>))[0]!!)).toInt()
    var a01: Int = (((((a[0]!!) as MutableList<Int>))[0]!! * (((b[0]!!) as MutableList<Int>))[1]!!) + ((((a[0]!!) as MutableList<Int>))[1]!! * (((b[1]!!) as MutableList<Int>))[1]!!)).toInt()
    var a10: Int = (((((a[1]!!) as MutableList<Int>))[0]!! * (((b[0]!!) as MutableList<Int>))[0]!!) + ((((a[1]!!) as MutableList<Int>))[1]!! * (((b[1]!!) as MutableList<Int>))[0]!!)).toInt()
    var a11: Int = (((((a[1]!!) as MutableList<Int>))[0]!! * (((b[0]!!) as MutableList<Int>))[1]!!) + ((((a[1]!!) as MutableList<Int>))[1]!! * (((b[1]!!) as MutableList<Int>))[1]!!)).toInt()
    return mutableListOf(mutableListOf(a00, a01), mutableListOf(a10, a11))
}

fun matrix_pow(m: MutableList<MutableList<Int>>, power: Int): MutableList<MutableList<Int>> {
    if (power < 0) {
        panic("power is negative")
    }
    var result: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 0), mutableListOf(0, 1))
    var base: MutableList<MutableList<Int>> = m
    var p: Int = (power).toInt()
    while (p > 0) {
        if ((Math.floorMod(p, 2)) == 1) {
            result = matrix_mul(result, base)
        }
        base = matrix_mul(base, base)
        p = ((p / 2).toInt())
    }
    return result
}

fun fib_matrix(n: Int): Int {
    if (n < 0) {
        panic("n is negative")
    }
    if (n == 0) {
        return 0
    }
    var m: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 1), mutableListOf(1, 0))
    var res: MutableList<MutableList<Int>> = matrix_pow(m, n - 1)
    return (((res[0]!!) as MutableList<Int>))[0]!!
}

fun run_tests(): Int {
    var expected: MutableList<Int> = mutableListOf(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
    var it: MutableList<Int> = fib_iterative(10)
    var rec: MutableList<Int> = fib_recursive(10)
    var cache: MutableList<Int> = fib_recursive_cached(10)
    var memo: MutableList<Int> = fib_memoization(10)
    var bin: MutableList<Int> = fib_binet(10)
    var m: Int = (fib_matrix(10)).toInt()
    if (it != expected) {
        panic("iterative failed")
    }
    if (rec != expected) {
        panic("recursive failed")
    }
    if (cache != expected) {
        panic("cached failed")
    }
    if (memo != expected) {
        panic("memoization failed")
    }
    if (bin != expected) {
        panic("binet failed")
    }
    if (m != 55) {
        panic("matrix failed")
    }
    return m
}

fun main() {
    println(run_tests().toString())
}
