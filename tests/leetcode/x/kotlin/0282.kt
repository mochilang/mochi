private fun solve(num: String, target: Long): List<String> {
    val ans = ArrayList<String>()
    fun dfs(i: Int, expr: String, value: Long, last: Long) {
        if (i == num.length) {
            if (value == target) ans.add(expr)
            return
        }
        for (j in i until num.length) {
            if (j > i && num[i] == '0') break
            val s = num.substring(i, j + 1)
            val n = s.toLong()
            if (i == 0) {
                dfs(j + 1, s, n, n)
            } else {
                dfs(j + 1, "$expr+$s", value + n, n)
                dfs(j + 1, "$expr-$s", value - n, -n)
                dfs(j + 1, "$expr*$s", value - last + last * n, last * n)
            }
        }
    }
    dfs(0, "", 0, 0)
    ans.sort()
    return ans
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val blocks = ArrayList<String>()
    var idx = 1
    repeat(t) {
        val num = lines[idx++].trim()
        val target = lines[idx++].trim().toLong()
        val ans = solve(num, target)
        blocks.add((listOf(ans.size.toString()) + ans).joinToString("\n"))
    }
    print(blocks.joinToString("\n\n"))
}
