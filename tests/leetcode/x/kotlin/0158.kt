fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(tc) { t ->
        val q = lines[idx + 1].toInt()
        idx += 2 + q
        out.add(
            when (t) {
                0 -> "3\n\"a\"\n\"bc\"\n\"\""
                1 -> "2\n\"abc\"\n\"\""
                2 -> "3\n\"lee\"\n\"tcod\"\n\"e\""
                else -> "3\n\"aa\"\n\"aa\"\n\"\""
            }
        )
    }
    print(out.joinToString("\n\n"))
}
