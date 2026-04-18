fun solve(n: Int): List<List<String>> {
    val cols = BooleanArray(n)
    val d1 = BooleanArray(2 * n)
    val d2 = BooleanArray(2 * n)
    val board = Array(n) { CharArray(n) { '.' } }
    val res = ArrayList<List<String>>()
    fun dfs(r: Int) {
        if (r == n) { res.add(board.map { String(it) }); return }
        for (c in 0 until n) {
            val a = r + c; val b = r - c + n - 1
            if (cols[c] || d1[a] || d2[b]) continue
            cols[c] = true; d1[a] = true; d2[b] = true; board[r][c] = 'Q'
            dfs(r + 1)
            board[r][c] = '.'; cols[c] = false; d1[a] = false; d2[b] = false
        }
    }
    dfs(0); return res
}
fun main() {
    val lines = generateSequence(::readLine).map { it.trim() }.toList()
    if (lines.isEmpty() || lines[0].isEmpty()) return
    var idx = 0
    val t = lines[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) { tc ->
        val n = lines[idx++].toInt()
        val sols = solve(n)
        out.add(sols.size.toString())
    }
    print(out.joinToString("\n"))
}
