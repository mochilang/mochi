private fun longest(s: String): Int {
    val last = mutableMapOf<Char, Int>()
    var left = 0
    var best = 0
    for (right in s.indices) {
        val ch = s[right]
        val prev = last[ch]
        if (prev != null && prev >= left) left = prev + 1
        last[ch] = right
        best = maxOf(best, right - left + 1)
    }
    return best
}

fun main() {
    val all = generateSequence { readLine() }.toList()
    if (all.isEmpty()) return
    val t = all[0].trim().toInt()
    val out = (0 until t).map { i -> longest(if (i + 1 < all.size) all[i + 1] else "").toString() }
    print(out.joinToString("\n"))
}
