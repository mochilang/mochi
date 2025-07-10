fun len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> 0
}

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val result = twoSum(mutableListOf(2, 7, 11, 15), 9)

fun twoSum(nums: MutableList<Int>, target: Int): MutableList<Int> {
    val n = len(nums)
    for (i in 0 until n) {
        for (j in i + 1 until n) {
            if (toBool(nums[i] + nums[j] == target)) {
                return mutableListOf(i, j)
            }
        }
    }
    return mutableListOf(-1, -1)
}

fun main() {
    println(result[0])
    println(result[1])
}
