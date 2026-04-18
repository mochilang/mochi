fun solveCase(s: String, words: List<String>): List<Int> {
    if (words.isEmpty()) return emptyList()
    val wlen = words[0].length
    val total = wlen * words.size
    val target = words.sorted()
    val ans = mutableListOf<Int>()
    for (i in 0..s.length - total) {
        val parts = MutableList(words.size) { j -> s.substring(i + j * wlen, i + (j + 1) * wlen) }.sorted()
        if (parts == target) ans.add(i)
    }
    return ans
}
fun main() {
    val lines = generateSequence(::readLine).toList(); if (lines.isEmpty()) return
    var idx = 0; val t = lines[idx++].trim().toInt(); val out = mutableListOf<String>()
    repeat(t) {
        val s = if (idx < lines.size) lines[idx++] else ""
        val m = if (idx < lines.size) lines[idx++].trim().toInt() else 0
        val words = MutableList(m) { if (idx < lines.size) lines[idx++] else "" }
        out.add("[" + solveCase(s, words).joinToString(",") + "]")
    }
    print(out.joinToString("\n"))
}
