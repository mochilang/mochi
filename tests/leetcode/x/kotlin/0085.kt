private fun hist(h: IntArray): Int {
    var best = 0
    for (i in h.indices) {
        var mn = h[i]
        for (j in i until h.size) {
            if (h[j] < mn) mn = h[j]
            val area = mn * (j - i + 1)
            if (area > best) best = area
        }
    }
    return best
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
        val h = IntArray(cols)
        var best = 0
        repeat(rows) {
            val s = toks[idx++]
            for (c in 0 until cols) h[c] = if (s[c] == '1') h[c] + 1 else 0
            best = maxOf(best, hist(h))
        }
        out.add(best.toString())
    }
    print(out.joinToString("\n"))
}
