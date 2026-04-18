fun justify(words: List<String>, maxWidth: Int): List<String> {
    val res = ArrayList<String>()
    var i = 0
    while (i < words.size) {
        var j = i
        var total = 0
        while (j < words.size && total + words[j].length + (j - i) <= maxWidth) {
            total += words[j].length
            j++
        }
        val gaps = j - i - 1
        val line = if (j == words.size || gaps == 0) {
            val joined = words.subList(i, j).joinToString(" ")
            joined + " ".repeat(maxWidth - joined.length)
        } else {
            val spaces = maxWidth - total
            val base = spaces / gaps
            val extra = spaces % gaps
            buildString {
                for (k in i until j - 1) {
                    append(words[k])
                    append(" ".repeat(base + if (k - i < extra) 1 else 0))
                }
                append(words[j - 1])
            }
        }
        res.add(line)
        i = j
    }
    return res
}
fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isNotEmpty()) {
        var idx = 0
        val t = lines[idx++].toInt()
        val out = ArrayList<String>()
        repeat(t) { tc ->
            val n = lines[idx++].toInt()
            val words = lines.subList(idx, idx + n)
            idx += n
            val width = lines[idx++].toInt()
            val ans = justify(words, width)
            out.add(ans.size.toString())
            ans.forEach { out.add("|$it|") }
            if (tc + 1 < t) out.add("=")
        }
        print(out.joinToString("\n"))
    }
}
