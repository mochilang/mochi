private fun convertZigzag(s: String, numRows: Int): String {
    if (numRows <= 1 || numRows >= s.length) return s
    val cycle = 2 * numRows - 2
    val out = StringBuilder()
    for (row in 0 until numRows) {
        var i = row
        while (i < s.length) {
            out.append(s[i])
            val diag = i + cycle - 2 * row
            if (row > 0 && row < numRows - 1 && diag < s.length) out.append(s[diag])
            i += cycle
        }
    }
    return out.toString()
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = mutableListOf<String>()
    var idx = 1
    repeat(t) {
        val s = if (idx < lines.size) lines[idx] else ""
        idx += 1
        val numRows = if (idx < lines.size) lines[idx].trim().toInt() else 1
        idx += 1
        out.add(convertZigzag(s, numRows))
    }
    print(out.joinToString("\n"))
}
