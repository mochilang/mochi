private fun twoSum(nums: IntArray, target: Int): Pair<Int, Int> {
    for (i in nums.indices) {
        for (j in i + 1 until nums.size) {
            if (nums[i] + nums[j] == target) {
                return Pair(i, j)
            }
        }
    }
    return Pair(0, 0)
}

private class FastScanner {
    private val data = generateSequence { readLine() }.joinToString(" ").trim()
    private val parts = if (data.isEmpty()) emptyList() else data.split(Regex("\\s+"))
    private var index = 0

    fun hasNext(): Boolean = index < parts.size
    fun nextInt(): Int = parts[index++].toInt()
}

fun main() {
    val fs = FastScanner()
    if (!fs.hasNext()) return
    val t = fs.nextInt()
    val out = StringBuilder()
    repeat(t) { tc ->
        val n = fs.nextInt()
        val target = fs.nextInt()
        val nums = IntArray(n) { fs.nextInt() }
        val ans = twoSum(nums, target)
        out.append(ans.first).append(' ').append(ans.second)
        if (tc + 1 < t) out.append('\n')
    }
    print(out.toString())
}
