fun matchAt(s: String, p: String, i: Int, j: Int): Boolean {
    if (j == p.length) return i == s.length
    val first = i < s.length && (p[j] == '.' || s[i] == p[j])
    if (j + 1 < p.length && p[j + 1] == '*') {
        return matchAt(s, p, i, j + 2) || (first && matchAt(s, p, i + 1, j))
    }
    return first && matchAt(s, p, i + 1, j + 1)
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(t) {
        val s = lines[idx++]
        val p = lines[idx++]
        out.add(if (matchAt(s, p, 0, 0)) "true" else "false")
    }
    print(out.joinToString("\n"))
}
