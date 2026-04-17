private fun lcp(strs: Array<String>): String {
    var prefix = strs[0]
    while (!strs.all { it.startsWith(prefix) }) {
        prefix = prefix.substring(0, prefix.length - 1)
    }
    return prefix
}

private class FastScanner {
    private val data = generateSequence { readLine() }.joinToString(" ").trim()
    private val parts = if (data.isEmpty()) emptyList() else data.split(Regex("\\s+"))
    private var index = 0
    fun hasNext(): Boolean = index < parts.size
    fun next(): String = parts[index++]
}

fun main() {
    val fs = FastScanner()
    if (!fs.hasNext()) return
    val t = fs.next().toInt()
    val out = StringBuilder()
    repeat(t) { tc ->
        val n = fs.next().toInt()
        val strs = Array(n) { fs.next() }
        out.append('"').append(lcp(strs)).append('"')
        if (tc + 1 < t) out.append('\n')
    }
    print(out.toString())
}
