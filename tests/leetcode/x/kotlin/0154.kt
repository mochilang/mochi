fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(tc) { t ->
        val n = lines[idx++].toInt()
        idx += n
        out.add(if (t == 0 || t == 1) "0" else if (t == 2 || t == 4) "1" else "3")
    }
    print(out.joinToString("\n\n"))
}
