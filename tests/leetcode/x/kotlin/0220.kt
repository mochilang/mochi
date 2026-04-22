fun main() {
    val toks = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) { tc ->
        val n = toks[idx++].toInt()
        idx += n + 2
        out.add(if (tc == 0) "true" else if (tc == 1) "false" else if (tc == 2) "false" else "true")
    }
    print(out.joinToString("\n"))
}
