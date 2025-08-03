fun binom(n: Int, k: Int): Int {
    if ((k < 0) || (k > n)) {
        return 0
    }
    var kk: Int = k
    if (kk > (n - kk)) {
        kk = n - kk
    }
    var res: Int = 1
    var i: Int = 0
    while (i < kk) {
        res = res * (n - i)
        i = i + 1
        res = ((res / i).toInt())
    }
    return res
}

fun catalan(n: Int): Int {
    return ((binom(2 * n, n) / (n + 1)).toInt())
}

fun user_main(): Unit {
    for (i in 0 until 15) {
        println(catalan(i).toString())
    }
}

fun main() {
    user_main()
}
