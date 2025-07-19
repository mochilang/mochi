// Mochi 0.10.31 - generated 2025-07-19 14:33:29 UTC
fun twoSum(nums: MutableList<Int>, target: Int): MutableList<Int> {
    val n = nums.size
    for (i in 0 until n) {
    for (j in (i + 1) until n) {
    if (((nums[i] + nums[j]) == target)) {
    return mutableListOf(i, j)
}
}
}
    return mutableListOf((0 - 1), (0 - 1))
}

fun main() {
    val result = twoSum(mutableListOf(2, 7, 11, 15), 9)
    println(result[0])
    println(result[1])
}
