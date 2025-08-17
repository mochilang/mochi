import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun is_happy_number(num: Int): Boolean {
    if (num <= 0) {
        panic("num must be a positive integer")
    }
    var seen: MutableList<Int> = mutableListOf<Int>()
    var n: Int = (num).toInt()
    while (n != 1) {
        var i: Int = (0).toInt()
        while (i < seen.size) {
            if (seen[i]!! == n) {
                return false
            }
            i = i + 1
        }
        seen = run { val _tmp = seen.toMutableList(); _tmp.add(n); _tmp }
        var total: Int = (0).toInt()
        var temp: Int = (n).toInt()
        while (temp > 0) {
            var digit: Int = (Math.floorMod(temp, 10)).toInt()
            total = total + (digit * digit)
            temp = temp / 10
        }
        n = total
    }
    return true
}

fun test_is_happy_number(): Unit {
    if (!is_happy_number(19)) {
        panic("19 should be happy")
    }
    if ((is_happy_number(2)) as Boolean) {
        panic("2 should be unhappy")
    }
    if (!is_happy_number(23)) {
        panic("23 should be happy")
    }
    if (!is_happy_number(1)) {
        panic("1 should be happy")
    }
}

fun user_main(): Unit {
    test_is_happy_number()
    println(is_happy_number(19))
}

fun main() {
    user_main()
}
