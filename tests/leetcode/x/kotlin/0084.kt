private fun solve(a: IntArray): Int {
    var best = 0
    for (i in a.indices) {
        var mn = a[i]
        for (j in i until a.size) {
            if (a[j] < mn) mn = a[j]
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
        val n = toks[idx++].toInt()
        val a = IntArray(n)
        for (i in 0 until n) a[i] = toks[idx++].toInt()
        out.add(solve(a).toString())
    }
    print(out.joinToString("\n"))
}
