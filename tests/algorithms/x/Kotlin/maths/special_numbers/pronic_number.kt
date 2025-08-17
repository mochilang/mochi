fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun int_sqrt(n: Int): Int {
    var r: Int = (0).toInt()
    while (((r + 1) * (r + 1)) <= n) {
        r = r + 1
    }
    return r
}

fun is_pronic(n: Int): Boolean {
    if (n < 0) {
        return false
    }
    if ((Math.floorMod(n, 2)) != 0) {
        return false
    }
    var root: Int = (int_sqrt(n)).toInt()
    return n == (root * (root + 1))
}

fun test_is_pronic(): Unit {
    if ((is_pronic(0 - 1)) as Boolean) {
        panic("-1 should not be pronic")
    }
    if (!is_pronic(0)) {
        panic("0 should be pronic")
    }
    if (!is_pronic(2)) {
        panic("2 should be pronic")
    }
    if ((is_pronic(5)) as Boolean) {
        panic("5 should not be pronic")
    }
    if (!is_pronic(6)) {
        panic("6 should be pronic")
    }
    if ((is_pronic(8)) as Boolean) {
        panic("8 should not be pronic")
    }
    if (!is_pronic(30)) {
        panic("30 should be pronic")
    }
    if ((is_pronic(32)) as Boolean) {
        panic("32 should not be pronic")
    }
    if (!is_pronic(2147441940)) {
        panic("2147441940 should be pronic")
    }
}

fun user_main(): Unit {
    test_is_pronic()
    println(is_pronic(56))
}

fun main() {
    user_main()
}
