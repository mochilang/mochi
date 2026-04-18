private fun addLists(a: List<Int>, b: List<Int>): List<Int> {
    val out = mutableListOf<Int>()
    var i = 0
    var j = 0
    var carry = 0
    while (i < a.size || j < b.size || carry > 0) {
        var sum = carry
        if (i < a.size) sum += a[i++]
        if (j < b.size) sum += b[j++]
        out.add(sum % 10)
        carry = sum / 10
    }
    return out
}

private fun format(a: List<Int>) = "[" + a.joinToString(",") + "]"

private class FastScanner {
    private val data = generateSequence { readLine() }.joinToString(" ").trim()
    private val parts = if (data.isEmpty()) emptyList() else data.split(Regex("\\s+"))
    private var index = 0
    fun hasNext() = index < parts.size
    fun next() = parts[index++]
}

fun main() {
    val fs = FastScanner()
    if (!fs.hasNext()) return
    val t = fs.next().toInt()
    val out = StringBuilder()
    repeat(t) { tc ->
        val n = fs.next().toInt()
        val a = MutableList(n) { fs.next().toInt() }
        val m = fs.next().toInt()
        val b = MutableList(m) { fs.next().toInt() }
        out.append(format(addLists(a, b)))
        if (tc + 1 < t) out.append('\n')
    }
    print(out.toString())
}
