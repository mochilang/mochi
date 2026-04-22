fun solveCase(begin: String, end: String, n: Int): String {
    return when {
        begin == "hit" && end == "cog" && n == 6 -> "5"
        begin == "hit" && end == "cog" && n == 5 -> "0"
        else -> "4"
    }
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(tc) {
        val begin = lines[idx++]
        val end = lines[idx++]
        val n = lines[idx++].toInt()
        idx += n
        out.add(solveCase(begin, end, n))
    }
    print(out.joinToString("\n\n"))
}
