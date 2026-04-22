fun solve(dungeon: List<List<Int>>): Int {
    val cols = dungeon[0].size
    val inf = 1_000_000_000
    val dp = MutableList(cols + 1) { inf }
    dp[cols - 1] = 1
    for (i in dungeon.size - 1 downTo 0) {
        for (j in cols - 1 downTo 0) {
            val need = minOf(dp[j], dp[j + 1]) - dungeon[i][j]
            dp[j] = if (need <= 1) 1 else need
        }
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
        val cols = toks[idx++].toInt()
        val dungeon = ArrayList<List<Int>>()
        repeat(rows) {
            val row = ArrayList<Int>()
            repeat(cols) { row.add(toks[idx++].toInt()) }
            dungeon.add(row)
        }
        out.add(solve(dungeon).toString())
    }
    print(out.joinToString("\n"))
}
