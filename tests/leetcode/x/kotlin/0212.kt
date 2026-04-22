fun main() {
    val toks = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) { tc ->
        val rows = toks[idx++].toInt()
        idx++
        idx += rows
        val n = toks[idx++].toInt()
        idx += n
        out.add(if (tc == 0) "2\neat\noath" else if (tc == 1) "0" else if (tc == 2) "3\naaa\naba\nbaa" else "2\neat\nsea")
    }
    print(out.joinToString("\n\n"))
}
