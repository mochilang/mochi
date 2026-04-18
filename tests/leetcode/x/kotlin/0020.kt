private fun isValid(s: String): Boolean {
    val stack = ArrayDeque<Char>()
    for (ch in s) {
        if (ch == '(' || ch == '[' || ch == '{') {
            stack.addLast(ch)
        } else {
            if (stack.isEmpty()) return false
            val open = stack.removeLast()
            if ((ch == ')' && open != '(') ||
                (ch == ']' && open != '[') ||
                (ch == '}' && open != '{')) return false
        }
    }
    return stack.isEmpty()
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
        out.append(if (isValid(fs.next())) "true" else "false")
        if (i + 1 < t) out.append('\n')
    }
    print(out.toString())
}
