fun solve(s1: String, s2: String, s3: String): Boolean {
    val m = s1.length
    val n = s2.length
    if (m + n != s3.length) return false
    val dp = Array(m + 1) { BooleanArray(n + 1) }
    dp[0][0] = true
    for (i in 0..m) for (j in 0..n) {
        if (i > 0 && dp[i - 1][j] && s1[i - 1] == s3[i + j - 1]) dp[i][j] = true
        if (j > 0 && dp[i][j - 1] && s2[j - 1] == s3[i + j - 1]) dp[i][j] = true
    }
    return dp[m][n]
}

fun main() {
    val lines = generateSequence { readLine() }.toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = ArrayList<String>()
    for (i in 0 until t) out.add(if (solve(lines[1 + 3 * i], lines[2 + 3 * i], lines[3 + 3 * i])) "true" else "false")
    print(out.joinToString("\n"))
}
