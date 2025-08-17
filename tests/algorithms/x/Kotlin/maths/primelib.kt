import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun abs_int(x: Int): Int {
    if (x < 0) {
        return 0 - x
    }
    return x
}

fun gcd_iter(a: Int, b: Int): Int {
    var x: Int = (abs_int(a)).toInt()
    var y: Int = (abs_int(b)).toInt()
    while (y != 0) {
        var t: Int = (y).toInt()
        y = Math.floorMod(x, y)
        x = t
    }
    return x
}

fun is_prime(n: Int): Boolean {
    if (n <= 1) {
        return false
    }
    var d: Int = (2).toInt()
    while ((d * d) <= n) {
        if ((Math.floorMod(n, d)) == 0) {
            return false
        }
        d = d + 1
    }
    return true
}

fun sieve_er(n: Int): MutableList<Int> {
    var nums: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (2).toInt()
    while (i <= n) {
        nums = run { val _tmp = nums.toMutableList(); _tmp.add(i); _tmp }
        i = i + 1
    }
    var idx: Int = (0).toInt()
    while (idx < nums.size) {
        var j: Int = (idx + 1).toInt()
        while (j < nums.size) {
            if (nums[idx]!! != 0) {
                if ((Math.floorMod(nums[j]!!, nums[idx]!!)) == 0) {
                    _listSet(nums, j, 0)
                }
            }
            j = j + 1
        }
        idx = idx + 1
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var k: Int = (0).toInt()
    while (k < nums.size) {
        var v: Int = (nums[k]!!).toInt()
        if (v != 0) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(v); _tmp }
        }
        k = k + 1
    }
    return res
}

fun get_prime_numbers(n: Int): MutableList<Int> {
    var ans: MutableList<Int> = mutableListOf<Int>()
    var num: Int = (2).toInt()
    while (num <= n) {
        if ((is_prime(num)) as Boolean) {
            ans = run { val _tmp = ans.toMutableList(); _tmp.add(num); _tmp }
        }
        num = num + 1
    }
    return ans
}

fun prime_factorization(number: Int): MutableList<Int> {
    if (number == 0) {
        return mutableListOf(0)
    }
    if (number == 1) {
        return mutableListOf(1)
    }
    var ans: MutableList<Int> = mutableListOf<Int>()
    if ((is_prime(number)) as Boolean) {
        ans = run { val _tmp = ans.toMutableList(); _tmp.add(number); _tmp }
        return ans
    }
    var quotient: Int = (number).toInt()
    var factor: Int = (2).toInt()
    while (quotient != 1) {
        if (is_prime(factor) && ((Math.floorMod(quotient, factor)) == 0)) {
            ans = run { val _tmp = ans.toMutableList(); _tmp.add(factor); _tmp }
            quotient = quotient / factor
        } else {
            factor = factor + 1
        }
    }
    return ans
}

fun greatest_prime_factor(number: Int): Int {
    var factors: MutableList<Int> = prime_factorization(number)
    var m: Int = (factors[0]!!).toInt()
    var i: Int = (1).toInt()
    while (i < factors.size) {
        if (factors[i]!! > m) {
            m = factors[i]!!
        }
        i = i + 1
    }
    return m
}

fun smallest_prime_factor(number: Int): Int {
    var factors: MutableList<Int> = prime_factorization(number)
    var m: Int = (factors[0]!!).toInt()
    var i: Int = (1).toInt()
    while (i < factors.size) {
        if (factors[i]!! < m) {
            m = factors[i]!!
        }
        i = i + 1
    }
    return m
}

fun kg_v(number1: Int, number2: Int): Int {
    if ((number1 < 1) || (number2 < 1)) {
        panic("numbers must be positive")
    }
    var g: Int = (gcd_iter(number1, number2)).toInt()
    return (number1 / g) * number2
}

fun is_even(number: Int): Boolean {
    return (Math.floorMod(number, 2)) == 0
}

fun is_odd(number: Int): Boolean {
    return (Math.floorMod(number, 2)) != 0
}

fun goldbach(number: Int): MutableList<Int> {
    if ((!is_even(number) as Boolean) || (number <= 2)) {
        panic("number must be even and > 2")
    }
    var primes: MutableList<Int> = get_prime_numbers(number)
    var i: Int = (0).toInt()
    while (i < primes.size) {
        var j: Int = (i + 1).toInt()
        while (j < primes.size) {
            if ((primes[i]!! + primes[j]!!) == number) {
                return mutableListOf(primes[i]!!, primes[j]!!)
            }
            j = j + 1
        }
        i = i + 1
    }
    return mutableListOf<Int>()
}

fun get_prime(n: Int): Int {
    if (n < 0) {
        panic("n must be non-negative")
    }
    var index: Int = (0).toInt()
    var ans: Int = (2).toInt()
    while (index < n) {
        index = index + 1
        ans = ans + 1
        while (!is_prime(ans)) {
            ans = ans + 1
        }
    }
    return ans
}

fun get_primes_between(p1: Int, p2: Int): MutableList<Int> {
    var bad1: Boolean = !is_prime(p1) as Boolean
    var bad2: Boolean = !is_prime(p2) as Boolean
    if (((bad1 || bad2 as Boolean)) || (p1 >= p2)) {
        panic("arguments must be prime and p1 < p2")
    }
    var num: Int = (p1 + 1).toInt()
    while (num < p2) {
        if ((is_prime(num)) as Boolean) {
            break
        }
        num = num + 1
    }
    var ans: MutableList<Int> = mutableListOf<Int>()
    while (num < p2) {
        ans = run { val _tmp = ans.toMutableList(); _tmp.add(num); _tmp }
        num = num + 1
        while (num < p2) {
            if ((is_prime(num)) as Boolean) {
                break
            }
            num = num + 1
        }
    }
    return ans
}

fun get_divisors(n: Int): MutableList<Int> {
    if (n < 1) {
        panic("n must be >= 1")
    }
    var ans: MutableList<Int> = mutableListOf<Int>()
    var d: Int = (1).toInt()
    while (d <= n) {
        if ((Math.floorMod(n, d)) == 0) {
            ans = run { val _tmp = ans.toMutableList(); _tmp.add(d); _tmp }
        }
        d = d + 1
    }
    return ans
}

fun is_perfect_number(number: Int): Boolean {
    if (number <= 1) {
        panic("number must be > 1")
    }
    var divisors: MutableList<Int> = get_divisors(number)
    var sum: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < (divisors.size - 1)) {
        sum = sum + divisors[i]!!
        i = i + 1
    }
    return sum == number
}

fun simplify_fraction(numerator: Int, denominator: Int): MutableList<Int> {
    if (denominator == 0) {
        panic("denominator cannot be zero")
    }
    var g: Int = (gcd_iter(abs_int(numerator), abs_int(denominator))).toInt()
    return mutableListOf(numerator / g, denominator / g)
}

fun factorial(n: Int): Int {
    if (n < 0) {
        panic("n must be >= 0")
    }
    var ans: Int = (1).toInt()
    var i: Int = (1).toInt()
    while (i <= n) {
        ans = ans * i
        i = i + 1
    }
    return ans
}

fun fib(n: Int): Int {
    if (n < 0) {
        panic("n must be >= 0")
    }
    if (n <= 1) {
        return 1
    }
    var tmp: Int = (0).toInt()
    var fib1: Int = (1).toInt()
    var ans: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < (n - 1)) {
        tmp = ans
        ans = ans + fib1
        fib1 = tmp
        i = i + 1
    }
    return ans
}

fun main() {
    println(is_prime(97).toString())
    println(sieve_er(20).toString())
    println(get_prime_numbers(20).toString())
    println(prime_factorization(287).toString())
    println(_numToStr(greatest_prime_factor(287)))
    println(_numToStr(smallest_prime_factor(287)))
    println(_numToStr(kg_v(8, 10)))
    println(goldbach(28).toString())
    println(_numToStr(get_prime(8)))
    println(get_primes_between(3, 23).toString())
    println(get_divisors(28).toString())
    println(is_perfect_number(28).toString())
    println(simplify_fraction(10, 20).toString())
    println(_numToStr(factorial(5)))
    println(_numToStr(fib(10)))
}
