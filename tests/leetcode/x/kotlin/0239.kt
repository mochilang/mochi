private fun solve(nums: IntArray, k: Int): IntArray {
    val dq = ArrayDeque<Int>()
    val ans = ArrayList<Int>()
    for (i in nums.indices) {
        while (dq.isNotEmpty() && dq.first() <= i - k) dq.removeFirst()
        while (dq.isNotEmpty() && nums[dq.last()] <= nums[i]) dq.removeLast()
        dq.addLast(i)
        if (i >= k - 1) ans.add(nums[dq.first()])
    }
    return ans.toIntArray()
}

fun main() {
    val toks = generateSequence(::readLine).flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val blocks = ArrayList<String>()
    repeat(t) {
        val n = toks[idx++].toInt()
        val nums = IntArray(n) { toks[idx++].toInt() }
        val k = toks[idx++].toInt()
        val ans = solve(nums, k)
        blocks.add((listOf(ans.size.toString()) + ans.map { it.toString() }).joinToString("\n"))
    }
    print(blocks.joinToString("\n\n"))
}
