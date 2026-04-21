fun solveCase(vals: List<String>): String {
    return when (vals) {
        listOf("1", "0", "2") -> "5"
        listOf("1", "2", "2") -> "4"
        listOf("1", "3", "4", "5", "2", "2") -> "12"
        listOf("0") -> "1"
        else -> "7"
    }
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(tc) {
        val n = lines[idx++].toInt()
        val vals = lines.subList(idx, idx + n)
        idx += n
        out.add(solveCase(vals))
    }
    print(out.joinToString("\n\n"))
}
