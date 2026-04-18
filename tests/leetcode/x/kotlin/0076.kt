fun minWindow(s: String, t: String): String {
    val need = IntArray(128)
    var missing = t.length
    for (ch in t) need[ch.code]++
    var left = 0
    var bestStart = 0
    var bestLen = s.length + 1
    for (right in s.indices) {
        val c = s[right].code
        if (need[c] > 0) missing--
        need[c]--
        while (missing == 0) {
            if (right - left + 1 < bestLen) { bestStart = left; bestLen = right - left + 1 }
            val lc = s[left].code
            need[lc]++
            if (need[lc] > 0) missing++
            left++
        }
    }
    return if (bestLen > s.length) "" else s.substring(bestStart, bestStart + bestLen)
}
fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isNotEmpty()) {
        val t = lines[0].toInt()
        val out = ArrayList<String>()
        repeat(t) { i -> out.add(minWindow(lines[1 + 2*i], lines[2 + 2*i])) }
        print(out.joinToString("\n"))
    }
}
