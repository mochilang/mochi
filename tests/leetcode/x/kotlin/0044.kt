fun isMatch(s: String, p: String): Boolean {
    var i = 0
    var j = 0
    var star = -1
    var match = 0
    while (i < s.length) {
        if (j < p.length && (p[j] == '?' || p[j] == s[i])) { i++; j++ }
        else if (j < p.length && p[j] == '*') { star = j; match = i; j++ }
        else if (star != -1) { j = star + 1; match++; i = match }
        else return false
    }
    while (j < p.length && p[j] == '*') j++
    return j == p.length
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty() || lines[0].trim().isEmpty()) return
    var idx = 0
    val t = lines[idx++].trim().toInt()
    val out = ArrayList<String>()
    repeat(t) {
        val n = lines[idx++].trim().toInt()
        val s = if (n > 0) lines[idx++] else ""
        val m = lines[idx++].trim().toInt()
        val p = if (m > 0) lines[idx++] else ""
        out.add(if (isMatch(s, p)) "true" else "false")
    }
    print(out.joinToString("\n"))
}
