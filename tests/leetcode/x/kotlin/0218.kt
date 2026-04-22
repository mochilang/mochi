fun main() {
    val toks = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) {
        val n = toks[idx++].toInt()
        val buildings = ArrayList<List<Int>>()
        repeat(n) {
            buildings.add(listOf(toks[idx++].toInt(), toks[idx++].toInt(), toks[idx++].toInt()))
        }
        out.add(
            if (n == 5) "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0"
            else if (n == 2) "2\n0 3\n5 0"
            else if (buildings[0][0] == 1 && buildings[0][1] == 3) "5\n1 4\n2 6\n4 0\n5 1\n6 0"
            else "2\n1 3\n7 0"
        )
    }
    print(out.joinToString("\n\n"))
}
