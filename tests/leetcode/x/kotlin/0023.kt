fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    var idx = 0
    val t = lines[idx++].trim().toInt()
    val out = mutableListOf<String>()
    repeat(t) {
        val k = if (idx < lines.size) lines[idx++].trim().toInt() else 0
        val vals = mutableListOf<Int>()
        repeat(k) {
            val n = if (idx < lines.size) lines[idx++].trim().toInt() else 0
            repeat(n) { vals.add(if (idx < lines.size) lines[idx++].trim().toInt() else 0) }
        }
        vals.sort()
        out.add("[" + vals.joinToString(",") + "]")
    }
    print(out.joinToString("\n"))
}
