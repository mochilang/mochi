fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun totient(n: Int): MutableList<Int> {
    var is_prime: MutableList<Boolean> = mutableListOf<Boolean>()
    var totients: MutableList<Int> = mutableListOf<Int>()
    var primes: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= n) {
        is_prime = run { val _tmp = is_prime.toMutableList(); _tmp.add(true); _tmp }
        totients = run { val _tmp = totients.toMutableList(); _tmp.add(i - 1); _tmp }
        i = i + 1
    }
    i = 2
    while (i <= n) {
        if (((is_prime[i]!!) as Boolean)) {
            primes = run { val _tmp = primes.toMutableList(); _tmp.add(i); _tmp }
        }
        var j: Int = (0).toInt()
        while (j < primes.size) {
            var p: Int = (primes[j]!!).toInt()
            if ((i * p) >= n) {
                break
            }
            _listSet(is_prime, i * p, false)
            if ((Math.floorMod(i, p)) == 0) {
                _listSet(totients, i * p, totients[i]!! * p)
                break
            }
            _listSet(totients, i * p, totients[i]!! * (p - 1))
            j = j + 1
        }
        i = i + 1
    }
    return totients
}

fun test_totient(): Unit {
    var expected: MutableList<Int> = mutableListOf(0 - 1, 0, 1, 2, 2, 4, 2, 6, 4, 6, 9)
    var res: MutableList<Int> = totient(10)
    var idx: Int = (0).toInt()
    while (idx < expected.size) {
        if (res[idx]!! != expected[idx]!!) {
            panic("totient mismatch at " + idx.toString())
        }
        idx = idx + 1
    }
}

fun user_main(): Unit {
    test_totient()
    var n: Int = (10).toInt()
    var res: MutableList<Int> = totient(n)
    var i: Int = (1).toInt()
    while (i < n) {
        println(((i.toString() + " has ") + (res[i]!!).toString()) + " relative primes.")
        i = i + 1
    }
}

fun main() {
    user_main()
}
