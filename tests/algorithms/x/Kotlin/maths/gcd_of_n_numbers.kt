import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun gcd(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    while (y != 0) {
        var r: Int = (Math.floorMod(x, y)).toInt()
        x = y
        y = r
    }
    if (x < 0) {
        return 0 - x
    }
    return x
}

fun get_greatest_common_divisor(nums: MutableList<Int>): Int {
    if (nums.size == 0) {
        panic("at least one number is required")
    }
    var g: Int = (nums[0]!!).toInt()
    if (g <= 0) {
        panic("numbers must be integer and greater than zero")
    }
    var i: Int = (1).toInt()
    while (i < nums.size) {
        var n: Int = (nums[i]!!).toInt()
        if (n <= 0) {
            panic("numbers must be integer and greater than zero")
        }
        g = gcd(g, n)
        i = i + 1
    }
    return g
}

fun main() {
    println(get_greatest_common_divisor(mutableListOf(18, 45)).toString())
    println(get_greatest_common_divisor(mutableListOf(23, 37)).toString())
    println(get_greatest_common_divisor(mutableListOf(2520, 8350)).toString())
    println(get_greatest_common_divisor(mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)).toString())
}
