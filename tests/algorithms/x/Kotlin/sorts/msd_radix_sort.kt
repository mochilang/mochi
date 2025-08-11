import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var ex1: MutableList<Int> = mutableListOf(40, 12, 1, 100, 4)
var sorted1: MutableList<Int> = msd_radix_sort(ex1)
fun get_bit_length(n: Int): Int {
    if (n == 0) {
        return 1
    }
    var length: Int = (0).toInt()
    var num: Int = (n).toInt()
    while (num > 0) {
        length = length + 1
        num = num / 2
    }
    return length
}

fun max_bit_length(nums: MutableList<Int>): Int {
    var i: Int = (0).toInt()
    var max_len: Int = (0).toInt()
    while (i < nums.size) {
        var l: Int = (get_bit_length(nums[i]!!)).toInt()
        if (l > max_len) {
            max_len = l
        }
        i = i + 1
    }
    return max_len
}

fun get_bit(num: Int, pos: Int): Int {
    var n: Int = (num).toInt()
    var i: Int = (0).toInt()
    while (i < pos) {
        n = n / 2
        i = i + 1
    }
    return Math.floorMod(n, 2)
}

fun _msd_radix_sort(nums: MutableList<Int>, bit_position: Int): MutableList<Int> {
    if ((bit_position == 0) || (nums.size <= 1)) {
        return nums
    }
    var zeros: MutableList<Int> = mutableListOf<Int>()
    var ones: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < nums.size) {
        var num: Int = (nums[i]!!).toInt()
        if (get_bit(num, bit_position - 1) == 1) {
            ones = run { val _tmp = ones.toMutableList(); _tmp.add(num); _tmp }
        } else {
            zeros = run { val _tmp = zeros.toMutableList(); _tmp.add(num); _tmp }
        }
        i = i + 1
    }
    zeros = _msd_radix_sort(zeros, bit_position - 1)
    ones = _msd_radix_sort(ones, bit_position - 1)
    var res: MutableList<Int> = zeros
    i = 0
    while (i < ones.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(ones[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun msd_radix_sort(nums: MutableList<Int>): MutableList<Int> {
    if (nums.size == 0) {
        return mutableListOf<Int>()
    }
    var i: Int = (0).toInt()
    while (i < nums.size) {
        if (nums[i]!! < 0) {
            panic("All numbers must be positive")
        }
        i = i + 1
    }
    var bits: Int = (max_bit_length(nums)).toInt()
    var result: MutableList<Int> = _msd_radix_sort(nums, bits)
    return result
}

fun msd_radix_sort_inplace(nums: MutableList<Int>): MutableList<Int> {
    return msd_radix_sort(nums)
}

fun main() {
    println(sorted1.toString())
    var ex2: MutableList<Int> = mutableListOf<Int>()
    var sorted2: MutableList<Int> = msd_radix_sort(ex2)
    println(sorted2.toString())
    var ex3: MutableList<Int> = mutableListOf(123, 345, 123, 80)
    var sorted3: MutableList<Int> = msd_radix_sort(ex3)
    println(sorted3.toString())
    var ex4: MutableList<Int> = mutableListOf(1209, 834598, 1, 540402, 45)
    var sorted4: MutableList<Int> = msd_radix_sort(ex4)
    println(sorted4.toString())
}
