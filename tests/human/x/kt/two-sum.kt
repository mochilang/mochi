fun twoSum(nums: List<Int>, target: Int): List<Int> {
    val n = nums.size
    for (i in 0 until n) {
        for (j in i + 1 until n) {
            if (nums[i] + nums[j] == target) {
                return listOf(i, j)
            }
        }
    }
    return listOf(-1, -1)
}

fun main() {
    val result = twoSum(listOf(2, 7, 11, 15), 9)
    println(result[0])
    println(result[1])
}
