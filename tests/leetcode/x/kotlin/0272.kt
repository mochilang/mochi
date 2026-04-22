private fun solve(values: IntArray, target: Double, k: Int): IntArray {
    var right = 0
    while (right < values.size && values[right] < target) right++
    var left = right - 1
    val ans = ArrayList<Int>()
    while (ans.size < k) {
        when {
            left < 0 -> ans.add(values[right++])
            right >= values.size -> ans.add(values[left--])
            kotlin.math.abs(values[left] - target) <= kotlin.math.abs(values[right] - target) -> ans.add(values[left--])
            else -> ans.add(values[right++])
        }
    }
    return ans.toIntArray()
}

fun main() {
    val toks = generateSequence(::readLine).flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val blocks = ArrayList<String>()
    repeat(t) {
        val n = toks[idx++].toInt()
        val values = IntArray(n) { toks[idx++].toInt() }
        val target = toks[idx++].toDouble()
        val k = toks[idx++].toInt()
        val ans = solve(values, target, k)
        blocks.add((listOf(ans.size.toString()) + ans.map { it.toString() }).joinToString("\n"))
    }
    print(blocks.joinToString("\n\n"))
}
