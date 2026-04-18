private fun solve(vals: List<Int>, ok: List<Boolean>): Int {
    var best = -1_000_000_000
    fun dfs(i: Int): Int {
        if (i >= vals.size || !ok[i]) return 0
        val left = maxOf(0, dfs(2 * i + 1))
        val right = maxOf(0, dfs(2 * i + 2))
        best = maxOf(best, vals[i] + left + right)
        return vals[i] + maxOf(left, right)
    }
    dfs(0)
    return best
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].trim().toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(tc) {
        val n = lines[idx++].trim().toInt()
        val vals = MutableList(n) { 0 }
        val ok = MutableList(n) { false }
        repeat(n) { i ->
            val tok = lines[idx++].trim()
            if (tok != "null") { ok[i] = true; vals[i] = tok.toInt() }
        }
        out.add(solve(vals, ok).toString())
    }
    print(out.joinToString("\n"))
}
