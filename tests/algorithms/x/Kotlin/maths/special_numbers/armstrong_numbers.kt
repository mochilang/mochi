import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun pow_int(base: Int, exp: Int): Int {
    var result: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun armstrong_number(n: Int): Boolean {
    if (n < 1) {
        return false
    }
    var digits: Int = (0).toInt()
    var temp: Int = (n).toInt()
    while (temp > 0) {
        temp = temp / 10
        digits = digits + 1
    }
    var total: Int = (0).toInt()
    temp = n
    while (temp > 0) {
        var rem: Int = (Math.floorMod(temp, 10)).toInt()
        total = total + pow_int(rem, digits)
        temp = temp / 10
    }
    return total == n
}

fun pluperfect_number(n: Int): Boolean {
    if (n < 1) {
        return false
    }
    var digit_histogram: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < 10) {
        digit_histogram = run { val _tmp = digit_histogram.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var digit_total: Int = (0).toInt()
    var temp: Int = (n).toInt()
    while (temp > 0) {
        var rem: Int = (Math.floorMod(temp, 10)).toInt()
        _listSet(digit_histogram, rem, digit_histogram[rem]!! + 1)
        digit_total = digit_total + 1
        temp = temp / 10
    }
    var total: Int = (0).toInt()
    i = 0
    while (i < 10) {
        if (digit_histogram[i]!! > 0) {
            total = total + (digit_histogram[i]!! * pow_int(i, digit_total))
        }
        i = i + 1
    }
    return total == n
}

fun narcissistic_number(n: Int): Boolean {
    if (n < 1) {
        return false
    }
    var digits: Int = (0).toInt()
    var temp: Int = (n).toInt()
    while (temp > 0) {
        temp = temp / 10
        digits = digits + 1
    }
    temp = n
    var total: Int = (0).toInt()
    while (temp > 0) {
        var rem: Int = (Math.floorMod(temp, 10)).toInt()
        total = total + pow_int(rem, digits)
        temp = temp / 10
    }
    return total == n
}

fun main() {
    println(armstrong_number(371))
    println(armstrong_number(200))
    println(pluperfect_number(371))
    println(pluperfect_number(200))
    println(narcissistic_number(371))
    println(narcissistic_number(200))
}
