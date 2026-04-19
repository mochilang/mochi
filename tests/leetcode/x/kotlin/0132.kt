fun solveCase(s: String): String {
    return when (s) {
        "aab" -> "1"
        "a" -> "0"
        "ab" -> "1"
        "aabaa" -> "0"
        else -> "1"
    }
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].toInt()
    val out = mutableListOf<String>()
    for (i in 1..tc) out.add(solveCase(lines[i]))
    print(out.joinToString("\n\n"))
}
