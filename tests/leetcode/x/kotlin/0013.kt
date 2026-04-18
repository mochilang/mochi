private val values = mapOf('I' to 1, 'V' to 5, 'X' to 10, 'L' to 50, 'C' to 100, 'D' to 500, 'M' to 1000)

private fun romanToInt(s: String): Int {
    var total = 0
    for (i in s.indices) {
        val cur = values[s[i]]!!
        val next = if (i + 1 < s.length) values[s[i + 1]]!! else 0
        total += if (cur < next) -cur else cur
    }
    return total
}

private class FastScanner {
    private val data = generateSequence { readLine() }.joinToString(" ").trim()
    private val parts = if (data.isEmpty()) emptyList() else data.split(Regex("\\s+"))
    private var index = 0
    fun hasNext(): Boolean = index < parts.size
    fun next(): String = parts[index++]
}

fun main() {
    val fs = FastScanner()
    if (!fs.hasNext()) return
    val t = fs.next().toInt()
    val out = StringBuilder()
    repeat(t) { i ->
        out.append(romanToInt(fs.next()))
        if (i + 1 < t) out.append('\n')
    }
    print(out.toString())
}
