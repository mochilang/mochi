private fun maxArea(h: List<Int>): Int {
    var left = 0
    var right = h.size - 1
    var best = 0
    while (left < right) {
        val height = minOf(h[left], h[right])
        best = maxOf(best, (right - left) * height)
        if (h[left] < h[right]) left++ else right--
    }
    return best
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(t) {
        val n = lines[idx++].trim().toInt()
        val h = MutableList(n) { lines[idx++].trim().toInt() }
        out.add(maxArea(h).toString())
    }
    print(out.joinToString("\n"))
}
