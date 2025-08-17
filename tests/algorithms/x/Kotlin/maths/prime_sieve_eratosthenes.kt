import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun prime_sieve_eratosthenes(num: Int): MutableList<Int> {
    if (num <= 0) {
        panic("Input must be a positive integer")
    }
    var primes: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i <= num) {
        primes = run { val _tmp = primes.toMutableList(); _tmp.add(true); _tmp }
        i = i + 1
    }
    var p: Int = (2).toInt()
    while ((p * p) <= num) {
        if ((primes[p]!!) as Boolean) {
            var j: Int = (p * p).toInt()
            while (j <= num) {
                _listSet(primes, j, false)
                j = j + p
            }
        }
        p = p + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    var k: Int = (2).toInt()
    while (k <= num) {
        if ((primes[k]!!) as Boolean) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(k); _tmp }
        }
        k = k + 1
    }
    return result
}

fun list_eq(a: MutableList<Int>, b: MutableList<Int>): Boolean {
    if (a.size != b.size) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < a.size) {
        if (a[i]!! != b[i]!!) {
            return false
        }
        i = i + 1
    }
    return true
}

fun test_prime_sieve_eratosthenes(): Unit {
    if (!list_eq(prime_sieve_eratosthenes(10), mutableListOf(2, 3, 5, 7))) {
        panic("test 10 failed")
    }
    if (!list_eq(prime_sieve_eratosthenes(20), mutableListOf(2, 3, 5, 7, 11, 13, 17, 19))) {
        panic("test 20 failed")
    }
    if (!list_eq(prime_sieve_eratosthenes(2), mutableListOf(2))) {
        panic("test 2 failed")
    }
    if ((prime_sieve_eratosthenes(1)).size != 0) {
        panic("test 1 failed")
    }
}

fun user_main(): Unit {
    test_prime_sieve_eratosthenes()
    println(prime_sieve_eratosthenes(20).toString())
}

fun main() {
    user_main()
}
