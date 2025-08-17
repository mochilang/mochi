import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Long, v: T) { while (lst.size <= idx.toInt()) lst.add(v); lst[idx.toInt()] = v }

fun odd_sieve(num: Int): MutableList<Int> {
    if (num <= 2) {
        return mutableListOf<Int>()
    }
    if (num == 3) {
        return mutableListOf(2)
    }
    var size: Int = ((num / 2) - 1).toInt()
    var sieve: MutableList<Boolean> = mutableListOf<Boolean>()
    var idx: Int = (0).toInt()
    while (idx < size) {
        sieve = run { val _tmp = sieve.toMutableList(); _tmp.add(true); _tmp }
        idx = idx + 1
    }
    var i: Int = (3).toInt()
    while (((i).toLong() * (i).toLong()) <= num) {
        var s_idx: Int = ((i / 2) - 1).toInt()
        if ((sieve[s_idx]!!) as Boolean) {
            var j: Long = (i).toLong() * (i).toLong()
            while (j < num) {
                var j_idx: Long = (j / (2).toLong()) - (1).toLong()
                _listSet(sieve, j_idx, false)
                j = (j + ((2 * i).toLong())).toInt()
            }
        }
        i = i + 2
    }
    var primes: MutableList<Int> = mutableListOf(2)
    var n: Int = (3).toInt()
    var k: Int = (0).toInt()
    while (n < num) {
        if ((sieve[k]!!) as Boolean) {
            primes = run { val _tmp = primes.toMutableList(); _tmp.add(n); _tmp }
        }
        n = n + 2
        k = k + 1
    }
    return primes
}

fun main() {
    println(odd_sieve(2))
    println(odd_sieve(3))
    println(odd_sieve(10))
    println(odd_sieve(20))
}
