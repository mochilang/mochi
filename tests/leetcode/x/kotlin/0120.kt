fun solve(tri: List<List<Int>>): Int {
    val dp = tri.last().toMutableList()
    for (i in tri.size - 2 downTo 0) {
        for (j in 0..i) dp[j] = tri[i][j] + minOf(dp[j], dp[j + 1])
    }
    return dp[0]
}

fun main() {
    val toks = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) {
        val rows = toks[idx++].toInt()
        val tri = ArrayList<List<Int>>()
        for (r in 1..rows) {
            val row = ArrayList<Int>()
            repeat(r) { row.add(toks[idx++].toInt()) }
            tri.add(row)
        }
        out.add(solve(tri).toString())
    }
    print(out.joinToString("\n"))
}
