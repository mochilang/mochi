private fun expand(s: String, left0: Int, right0: Int): Pair<Int, Int> {
    var left = left0
    var right = right0
    while (left >= 0 && right < s.length && s[left] == s[right]) {
        left--
        right++
    }
    return Pair(left + 1, right - left - 1)
}

private fun longestPalindrome(s: String): String {
    var bestStart = 0
    var bestLen = if (s.isEmpty()) 0 else 1
    for (i in s.indices) {
        val odd = expand(s, i, i)
        if (odd.second > bestLen) {
            bestStart = odd.first
            bestLen = odd.second
        }
        val even = expand(s, i, i + 1)
        if (even.second > bestLen) {
            bestStart = even.first
            bestLen = even.second
        }
    }
    return s.substring(bestStart, bestStart + bestLen)
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = mutableListOf<String>()
    for (i in 0 until t) {
        val s = if (i + 1 < lines.size) lines[i + 1] else ""
        out.add(longestPalindrome(s))
    }
    print(out.joinToString("\n"))
}
