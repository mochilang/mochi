import java.util.TreeSet

fun dfs(s: String, i: Int, left: Int, right: Int, balance: Int, path: StringBuilder, ans: MutableSet<String>) {
    if (i == s.length) {
        if (left == 0 && right == 0 && balance == 0) ans.add(path.toString())
        return
    }
    val ch = s[i]
    val len = path.length
    when (ch) {
        '(' -> {
            if (left > 0) dfs(s, i + 1, left - 1, right, balance, path, ans)
            path.append(ch)
            dfs(s, i + 1, left, right, balance + 1, path, ans)
            path.setLength(len)
        }
        ')' -> {
            if (right > 0) dfs(s, i + 1, left, right - 1, balance, path, ans)
            if (balance > 0) {
                path.append(ch)
                dfs(s, i + 1, left, right, balance - 1, path, ans)
                path.setLength(len)
            }
        }
        else -> {
            path.append(ch)
            dfs(s, i + 1, left, right, balance, path, ans)
            path.setLength(len)
        }
    }
}

fun solve(s: String): List<String> {
    var leftRemove = 0
    var rightRemove = 0
    for (ch in s) {
        if (ch == '(') leftRemove++
        else if (ch == ')') {
            if (leftRemove > 0) leftRemove-- else rightRemove++
        }
    }
    val ans = TreeSet<String>()
    dfs(s, 0, leftRemove, rightRemove, 0, StringBuilder(), ans)
    return ans.toList()
}

fun main() {
    val lines = generateSequence { readLine() }.toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val blocks = mutableListOf<String>()
    repeat(t) { tc ->
        val ans = solve(lines[tc + 1])
        blocks.add((listOf(ans.size.toString()) + ans).joinToString("\n"))
    }
    print(blocks.joinToString("\n\n"))
}
