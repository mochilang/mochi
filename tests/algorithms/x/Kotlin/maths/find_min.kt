import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun find_min_iterative(nums: MutableList<Double>): Double {
    if (nums.size == 0) {
        panic("find_min_iterative() arg is an empty sequence")
    }
    var min_num: Double = nums[0]!!
    var i: Int = (0).toInt()
    while (i < nums.size) {
        var num: Double = nums[i]!!
        if (num < min_num) {
            min_num = num
        }
        i = i + 1
    }
    return min_num
}

fun find_min_recursive(nums: MutableList<Double>, left: Int, right: Int): Double {
    var n: Int = (nums.size).toInt()
    if (n == 0) {
        panic("find_min_recursive() arg is an empty sequence")
    }
    if ((((((left >= n) || (left < (0 - n)) as Boolean)) || (right >= n) as Boolean)) || (right < (0 - n))) {
        panic("list index out of range")
    }
    var l: Int = (left).toInt()
    var r: Int = (right).toInt()
    if (l < 0) {
        l = n + l
    }
    if (r < 0) {
        r = n + r
    }
    if (l == r) {
        return nums[l]!!
    }
    var mid: Int = ((l + r) / 2).toInt()
    var left_min: Double = find_min_recursive(nums, l, mid)
    var right_min: Double = find_min_recursive(nums, mid + 1, r)
    if (left_min <= right_min) {
        return left_min
    }
    return right_min
}

fun test_find_min(): Unit {
    var a: MutableList<Double> = mutableListOf(3.0, 2.0, 1.0)
    if (find_min_iterative(a) != 1.0) {
        panic("iterative test1 failed")
    }
    if (find_min_recursive(a, 0, a.size - 1) != 1.0) {
        panic("recursive test1 failed")
    }
    var b: MutableList<Double> = mutableListOf(0.0 - 3.0, 0.0 - 2.0, 0.0 - 1.0)
    if (find_min_iterative(b) != (0.0 - 3.0)) {
        panic("iterative test2 failed")
    }
    if (find_min_recursive(b, 0, b.size - 1) != (0.0 - 3.0)) {
        panic("recursive test2 failed")
    }
    var c: MutableList<Double> = mutableListOf(3.0, 0.0 - 3.0, 0.0)
    if (find_min_iterative(c) != (0.0 - 3.0)) {
        panic("iterative test3 failed")
    }
    if (find_min_recursive(c, 0, c.size - 1) != (0.0 - 3.0)) {
        panic("recursive test3 failed")
    }
    var d: MutableList<Double> = mutableListOf(1.0, 3.0, 5.0, 7.0, 9.0, 2.0, 4.0, 6.0, 8.0, 10.0)
    if (find_min_recursive(d, 0 - d.size, 0 - 1) != 1.0) {
        panic("negative index test failed")
    }
}

fun user_main(): Unit {
    test_find_min()
    var sample: MutableList<Double> = mutableListOf(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 0.0 - 3.0, 24.0, 0.0 - 56.0)
    println(find_min_iterative(sample).toString())
}

fun main() {
    user_main()
}
