fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun floor(x: Double): Int {
    var i: Int = ((x.toInt())).toInt()
    if ((x - ((i.toDouble()))) >= 0.0) {
        return i
    }
    return i - 1
}

fun test_floor(): Unit {
    var nums: MutableList<Double> = mutableListOf(1.0, 0.0 - 1.0, 0.0, 0.0, 1.1, 0.0 - 1.1, 1.0, 0.0 - 1.0, 1000000000.0)
    var expected: MutableList<Int> = mutableListOf(1, 0 - 1, 0, 0, 1, 0 - 2, 1, 0 - 1, 1000000000)
    var idx: Int = (0).toInt()
    while (idx < nums.size) {
        if (floor(nums[idx]!!) != expected[idx]!!) {
            panic("floor test failed")
        }
        idx = idx + 1
    }
}

fun user_main(): Unit {
    test_floor()
    println(floor(0.0 - 1.1).toString())
}

fun main() {
    user_main()
}
