import java.math.BigInteger

fun floor_div(a: Int, b: Int): Int {
    var q: Int = (a / b).toInt()
    var r: Int = (Math.floorMod(a, b)).toInt()
    if ((r != 0) && (((((a < 0) && (b > 0) as Boolean)) || (((a > 0) && (b < 0) as Boolean)) as Boolean))) {
        q = q - 1
    }
    return q
}

fun continued_fraction(numerator: Int, denominator: Int): MutableList<Int> {
    var num: Int = (numerator).toInt()
    var den: Int = (denominator).toInt()
    var result: MutableList<Int> = mutableListOf<Int>()
    while (true) {
        var integer_part: Int = (floor_div(num, den)).toInt()
        result = run { val _tmp = result.toMutableList(); _tmp.add(integer_part); _tmp }
        num = num - (integer_part * den)
        if (num == 0) {
            break
        }
        var tmp: Int = (num).toInt()
        num = den
        den = (tmp.toInt())
    }
    return result
}

fun list_to_string(lst: MutableList<Int>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < lst.size) {
        s = s + (lst[i]!!).toString()
        if (i < (lst.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    return s + "]"
}

fun main() {
    println("Continued Fraction of 0.84375 is: " + list_to_string(continued_fraction(27, 32)))
}
