import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun isqrt(n: Int): Int {
    var r: Int = (0).toInt()
    while (((r + 1) * (r + 1)) <= n) {
        r = r + 1
    }
    return r
}

fun prime_sieve(num: Int): MutableList<Int> {
    if (num <= 0) {
        panic("Invalid input, please enter a positive integer.")
    }
    var sieve: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i <= num) {
        sieve = run { val _tmp = sieve.toMutableList(); _tmp.add(true); _tmp }
        i = i + 1
    }
    var prime: MutableList<Int> = mutableListOf<Int>()
    var start: Int = (2).toInt()
    var end: Int = (isqrt(num)).toInt()
    while (start <= end) {
        if ((sieve[start]!!) as Boolean) {
            prime = run { val _tmp = prime.toMutableList(); _tmp.add(start); _tmp }
            var j: Int = (start * start).toInt()
            while (j <= num) {
                if ((sieve[j]!!) as Boolean) {
                    _listSet(sieve, j, false)
                }
                j = j + start
            }
        }
        start = start + 1
    }
    var k: Int = (end + 1).toInt()
    while (k <= num) {
        if ((sieve[k]!!) as Boolean) {
            prime = run { val _tmp = prime.toMutableList(); _tmp.add(k); _tmp }
        }
        k = k + 1
    }
    return prime
}

fun main() {
    println(prime_sieve(50).toString())
    println(prime_sieve(25).toString())
    println(prime_sieve(10).toString())
    println(prime_sieve(9).toString())
    println(prime_sieve(2).toString())
    println(prime_sieve(1).toString())
}
