import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun min_int(a: Int, b: Int): Int {
    if (a < b) {
        return a
    }
    return b
}

fun int_sqrt(n: Int): Int {
    var r: Int = (0).toInt()
    while (((r + 1) * (r + 1)) <= n) {
        r = r + 1
    }
    return r
}

fun sieve(n: Int): MutableList<Int> {
    if (n <= 0) {
        panic("Number must instead be a positive integer")
    }
    var in_prime: MutableList<Int> = mutableListOf<Int>()
    var start: Int = (2).toInt()
    var end: Int = (int_sqrt(n)).toInt()
    var temp: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < (end + 1)) {
        temp = run { val _tmp = temp.toMutableList(); _tmp.add(1); _tmp }
        i = i + 1
    }
    var prime: MutableList<Int> = mutableListOf<Int>()
    while (start <= end) {
        if (temp[start]!! == 1) {
            in_prime = run { val _tmp = in_prime.toMutableList(); _tmp.add(start); _tmp }
            var j: Int = (start * start).toInt()
            while (j <= end) {
                _listSet(temp, j, 0)
                j = j + start
            }
        }
        start = start + 1
    }
    i = 0
    while (i < in_prime.size) {
        prime = run { val _tmp = prime.toMutableList(); _tmp.add(in_prime[i]!!); _tmp }
        i = i + 1
    }
    var low: Int = (end + 1).toInt()
    var high: Int = (min_int(2 * end, n)).toInt()
    while (low <= n) {
        var tempSeg: MutableList<Int> = mutableListOf<Int>()
        var size: Int = ((high - low) + 1).toInt()
        var k: Int = (0).toInt()
        while (k < size) {
            tempSeg = run { val _tmp = tempSeg.toMutableList(); _tmp.add(1); _tmp }
            k = k + 1
        }
        var idx: Int = (0).toInt()
        while (idx < in_prime.size) {
            var each: Int = (in_prime[idx]!!).toInt()
            var t: Int = ((low / each) * each).toInt()
            if (t < low) {
                t = t + each
            }
            var j2: Int = (t).toInt()
            while (j2 <= high) {
                _listSet(tempSeg, j2 - low, 0)
                j2 = j2 + each
            }
            idx = idx + 1
        }
        var j3: Int = (0).toInt()
        while (j3 < tempSeg.size) {
            if (tempSeg[j3]!! == 1) {
                prime = run { val _tmp = prime.toMutableList(); _tmp.add(j3 + low); _tmp }
            }
            j3 = j3 + 1
        }
        low = high + 1
        high = min_int(high + end, n)
    }
    return prime
}

fun lists_equal(a: MutableList<Int>, b: MutableList<Int>): Boolean {
    if (a.size != b.size) {
        return false
    }
    var m: Int = (0).toInt()
    while (m < a.size) {
        if (a[m]!! != b[m]!!) {
            return false
        }
        m = m + 1
    }
    return true
}

fun test_sieve(): Unit {
    var e1: MutableList<Int> = sieve(8)
    if (!lists_equal(e1, mutableListOf(2, 3, 5, 7))) {
        panic("sieve(8) failed")
    }
    var e2: MutableList<Int> = sieve(27)
    if (!lists_equal(e2, mutableListOf(2, 3, 5, 7, 11, 13, 17, 19, 23))) {
        panic("sieve(27) failed")
    }
}

fun user_main(): Unit {
    test_sieve()
    println(sieve(30).toString())
}

fun main() {
    user_main()
}
