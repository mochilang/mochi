import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun remove_digit(num: Int): Int {
    var n: Int = (num).toInt()
    if (n < 0) {
        n = 0 - n
    }
    var max_val: Int = (0).toInt()
    var divisor: Int = (1).toInt()
    while (divisor <= n) {
        var higher: Int = (n / (divisor * 10)).toInt()
        var lower: Int = (Math.floorMod(n, divisor)).toInt()
        var candidate: Int = ((higher * divisor) + lower).toInt()
        if (candidate > max_val) {
            max_val = candidate
        }
        divisor = divisor * 10
    }
    return max_val
}

fun test_remove_digit(): Unit {
    if (remove_digit(152) != 52) {
        panic("remove_digit(152) failed")
    }
    if (remove_digit(6385) != 685) {
        panic("remove_digit(6385) failed")
    }
    if (remove_digit(0 - 11) != 1) {
        panic("remove_digit(-11) failed")
    }
    if (remove_digit(2222222) != 222222) {
        panic("remove_digit(2222222) failed")
    }
}

fun user_main(): Unit {
    test_remove_digit()
    println(remove_digit(152))
}

fun main() {
    user_main()
}
