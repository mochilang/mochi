private fun solve(costs: Array<IntArray>): Int {
    if (costs.isEmpty()) return 0
    var prev = costs[0].clone()
    for (r in 1 until costs.size) {
        var min1 = Int.MAX_VALUE
        var min2 = Int.MAX_VALUE
        var idx1 = -1
        for (i in prev.indices) {
            if (prev[i] < min1) {
                min2 = min1
                min1 = prev[i]
                idx1 = i
            } else if (prev[i] < min2) {
                min2 = prev[i]
            }
        }
        val cur = IntArray(prev.size)
        for (i in prev.indices) {
            cur[i] = costs[r][i] + if (i == idx1) min2 else min1
        }
        prev = cur
    }
    return prev.min()
}

fun main() {
    val toks = generateSequence(::readLine).flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) {
        val n = toks[idx++].toInt()
        val k = toks[idx++].toInt()
        val costs = Array(n) { IntArray(k) }
        for (i in 0 until n) for (j in 0 until k) costs[i][j] = toks[idx++].toInt()
        out.add(solve(costs).toString())
    }
    print(out.joinToString("\n"))
}
