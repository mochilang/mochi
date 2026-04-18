private fun isPalindrome(x: Int): Boolean {
    if (x < 0) return false
    val original = x
    var n = x
    var rev = 0L
    while (n > 0) {
        rev = rev * 10 + (n % 10)
        n /= 10
    }
    return rev == original.toLong()
}

private class FastScanner {
    private val data = generateSequence { readLine() }.joinToString(" ").trim()
    private val parts = if (data.isEmpty()) emptyList() else data.split(Regex("\\s+"))
    private var index = 0
    fun hasNext(): Boolean = index < parts.size
    fun nextInt(): Int = parts[index++].toInt()
}

fun main() {
    val fs = FastScanner()
    if (!fs.hasNext()) return
    val t = fs.nextInt()
    val out = StringBuilder()
    repeat(t) { i ->
        out.append(if (isPalindrome(fs.nextInt())) "true" else "false")
        if (i + 1 < t) out.append('\n')
    }
    print(out.toString())
}
