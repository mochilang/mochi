fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun prime_factors(n: Int): MutableList<Int> {
    if (n < 2) {
        return mutableListOf<Int>()
    }
    var num: Int = (n).toInt()
    var i: Int = (2).toInt()
    var factors: MutableList<Int> = mutableListOf<Int>()
    while (((i).toLong() * (i).toLong()) <= num) {
        if ((Math.floorMod(num, i)) == 0) {
            factors = run { val _tmp = factors.toMutableList(); _tmp.add(i); _tmp }
            num = num / i
        } else {
            i = i + 1
        }
    }
    if (num > 1) {
        factors = run { val _tmp = factors.toMutableList(); _tmp.add(num); _tmp }
    }
    return factors
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

fun test_prime_factors(): Unit {
    if (!list_eq(prime_factors(0), mutableListOf<Int>())) {
        panic("prime_factors(0) failed")
    }
    if (!list_eq(prime_factors(100), mutableListOf(2, 2, 5, 5))) {
        panic("prime_factors(100) failed")
    }
    if (!list_eq(prime_factors(2560), mutableListOf(2, 2, 2, 2, 2, 2, 2, 2, 2, 5))) {
        panic("prime_factors(2560) failed")
    }
    if (!list_eq(prime_factors(97), mutableListOf(97))) {
        panic("prime_factors(97) failed")
    }
}

fun user_main(): Unit {
    test_prime_factors()
    println(prime_factors(100).toString())
    println(prime_factors(2560).toString())
    println(prime_factors(97).toString())
}

fun main() {
    user_main()
}
