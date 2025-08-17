import java.math.BigInteger

fun factorial(digit: Int): Int {
    if ((digit == 0) || (digit == 1)) {
        return 1
    }
    return digit * factorial(digit - 1)
}

fun is_krishnamurthy(n: Int): Boolean {
    var duplicate: Int = (n).toInt()
    var fact_sum: Int = (0).toInt()
    while (duplicate > 0) {
        var digit: Int = (Math.floorMod(duplicate, 10)).toInt()
        fact_sum = fact_sum + factorial(digit)
        duplicate = duplicate / 10
    }
    return fact_sum == n
}

fun main() {
    println(is_krishnamurthy(145).toString())
    println(is_krishnamurthy(240).toString())
    println(is_krishnamurthy(1).toString())
}
