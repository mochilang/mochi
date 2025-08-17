fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun ugly_numbers(n: Int): Int {
    if (n <= 0) {
        return 1
    }
    var ugly_nums: MutableList<Int> = mutableListOf<Int>()
    ugly_nums = run { val _tmp = ugly_nums.toMutableList(); _tmp.add(1); _tmp }
    var i2: Int = (0).toInt()
    var i3: Int = (0).toInt()
    var i5: Int = (0).toInt()
    var next_2: Int = (2).toInt()
    var next_3: Int = (3).toInt()
    var next_5: Int = (5).toInt()
    var count: Int = (1).toInt()
    while (count < n) {
        var next_num: Int = (if (next_2 < next_3) if (next_2 < next_5) next_2 else next_5 else if (next_3 < next_5) next_3 else next_5).toInt()
        ugly_nums = run { val _tmp = ugly_nums.toMutableList(); _tmp.add(next_num); _tmp }
        if (next_num == next_2) {
            i2 = i2 + 1
            next_2 = ugly_nums[i2]!! * 2
        }
        if (next_num == next_3) {
            i3 = i3 + 1
            next_3 = ugly_nums[i3]!! * 3
        }
        if (next_num == next_5) {
            i5 = i5 + 1
            next_5 = ugly_nums[i5]!! * 5
        }
        count = count + 1
    }
    return ugly_nums[ugly_nums.size - 1]!!
}

fun main() {
    println(_numToStr(ugly_numbers(100)))
    println(_numToStr(ugly_numbers(0)))
    println(_numToStr(ugly_numbers(20)))
    println(_numToStr(ugly_numbers(0 - 5)))
    println(_numToStr(ugly_numbers(200)))
}
