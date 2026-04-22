fun solve(s: String, t: String): Int {
    val dp = IntArray(t.length + 1)
    dp[0] = 1
    for (ch in s) {
        for (j in t.length downTo 1) {
            if (ch == t[j - 1]) dp[j] += dp[j - 1]
        }
    }
    return dp[t.length]
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].toInt()
    val out = mutableListOf<String>()
    for (i in 0 until tc) {
        out += solve(lines[1 + 2 * i], lines[2 + 2 * i]).toString()
    }
    print(out.joinToString("\n"))
}
