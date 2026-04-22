fun maxCoins(nums: IntArray): Int {
    val vals = IntArray(nums.size + 2)
    vals[0] = 1
    vals[vals.lastIndex] = 1
    for (i in nums.indices) vals[i + 1] = nums[i]
    val n = vals.size
    val dp = Array(n) { IntArray(n) }
    for (length in 2 until n) {
        for (left in 0 until n - length) {
            val right = left + length
            for (k in left + 1 until right) {
                dp[left][right] = maxOf(dp[left][right], dp[left][k] + dp[k][right] + vals[left] * vals[k] * vals[right])
            }
        }
    }
    return dp[0][n - 1]
}

fun main() {
    val data = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.map { it.toInt() }.toList()
    if (data.isEmpty()) return
    var idx = 0
    val t = data[idx++]
    val blocks = mutableListOf<String>()
    repeat(t) {
        val n = data[idx++]
        val nums = IntArray(n) { data[idx + it] }
        idx += n
        blocks.add(maxCoins(nums).toString())
    }
    print(blocks.joinToString("\n\n"))
}
