import java.math.BigInteger

fun pow(base: Int, exp: Int): Int {
    var result: Int = 1
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun isDisarium(n: Int): Boolean {
    var digits: MutableList<Int> = mutableListOf<Int>()
    var x: Int = n
    if (x == 0) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(0); _tmp }
    }
    while (x > 0) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(Math.floorMod(x, 10)); _tmp }
        x = ((x / 10).toInt())
    }
    var sum: Int = 0
    var pos: Int = 1
    var i: BigInteger = (digits.size - 1).toBigInteger()
    while (i.compareTo((0).toBigInteger()) >= 0) {
        sum = sum + pow(digits[(i).toInt()]!!, pos)
        pos = pos + 1
        i = i.subtract((1).toBigInteger())
    }
    return sum == n
}

fun user_main(): Unit {
    var count: Int = 0
    var n: Int = 0
    while ((count < 19) && (n < 3000000)) {
        if (((isDisarium(n)) as Boolean)) {
            println(n.toString())
            count = count + 1
        }
        n = n + 1
    }
    println(("\nFound the first " + count.toString()) + " Disarium numbers.")
}

fun main() {
    user_main()
}
