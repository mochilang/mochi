fun kPrime(n: Int, k: Int): Boolean {
    var n: Int = n
    var nf: Int = 0
    var i: Int = 2
    while (i <= n) {
        while ((n % i) == 0) {
            if (nf == k) {
                return false
            }
            nf = nf + 1
            n = n / i
        }
        i = i + 1
    }
    return nf == k
}

fun gen(k: Int, count: Int): MutableList<Int> {
    var r: MutableList<Int> = mutableListOf()
    var n: Int = 2
    while (r.size < count) {
        if (kPrime(n, k) as Boolean as Boolean) {
            r = run { val _tmp = r.toMutableList(); _tmp.add(n); _tmp } as MutableList<Int>
        }
        n = n + 1
    }
    return r
}

fun user_main(): Unit {
    var k: Int = 1
    while (k <= 5) {
        println((k.toString() + " ") + gen(k, 10) as MutableList<Int>.toString())
        k = k + 1
    }
}

fun main() {
    user_main()
}
