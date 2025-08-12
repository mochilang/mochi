import java.math.BigInteger

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun reverse(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: BigInteger = ((xs.size - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[(i).toInt()]!!); _tmp }
        i = i.subtract((1).toBigInteger())
    }
    return res
}

fun factors_of_a_number(num: Int): MutableList<Int> {
    var facs: MutableList<Int> = mutableListOf<Int>()
    if (num < 1) {
        return facs
    }
    var small: MutableList<Int> = mutableListOf<Int>()
    var large: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while ((i * i) <= num) {
        if ((Math.floorMod(num, i)) == 0) {
            small = run { val _tmp = small.toMutableList(); _tmp.add(i); _tmp }
            var d: Int = (num / i).toInt()
            if (d != i) {
                large = run { val _tmp = large.toMutableList(); _tmp.add(d); _tmp }
            }
        }
        i = i + 1
    }
    facs = ((concat(small, reverse(large))) as MutableList<Int>)
    return facs
}

fun run_tests(): Unit {
    if (factors_of_a_number(1) != mutableListOf(1)) {
        panic("case1 failed")
    }
    if (factors_of_a_number(5) != mutableListOf(1, 5)) {
        panic("case2 failed")
    }
    if (factors_of_a_number(24) != mutableListOf(1, 2, 3, 4, 6, 8, 12, 24)) {
        panic("case3 failed")
    }
    if (factors_of_a_number(0 - 24) != mutableListOf<Any?>()) {
        panic("case4 failed")
    }
}

fun user_main(): Unit {
    run_tests()
    println(factors_of_a_number(24).toString())
}

fun main() {
    user_main()
}
