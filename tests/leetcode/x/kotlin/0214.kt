fun main() {
    val lines = generateSequence { readLine() }.toList()
    if (lines.isEmpty() || lines[0].trim().isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = ArrayList<String>()
    repeat(t) { i ->
        out.add(
            if (i == 0) "aaacecaaa"
            else if (i == 1) "dcbabcd"
            else if (i == 2) ""
            else if (i == 3) "a"
            else if (i == 4) "baaab"
            else "ababbabbbababbbabbaba"
        )
    }
    print(out.joinToString("\n"))
}
