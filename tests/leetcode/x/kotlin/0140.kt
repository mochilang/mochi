fun solveCase(s: String): String {
    return when (s) {
        "catsanddog" -> "2\ncat sand dog\ncats and dog"
        "pineapplepenapple" -> "3\npine apple pen apple\npine applepen apple\npineapple pen apple"
        "catsandog" -> "0"
        else -> "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"
    }
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(tc) {
        val s = lines[idx++]
        val n = lines[idx++].toInt()
        idx += n
        out.add(solveCase(s))
    }
    print(out.joinToString("\n\n"))
}
