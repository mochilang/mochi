private fun maxProfit(prices: List<Int>): Int {
    if (prices.isEmpty()) return 0
    var minPrice = prices[0]
    var best = 0
    for (i in 1 until prices.size) {
        best = maxOf(best, prices[i] - minPrice)
        minPrice = minOf(minPrice, prices[i])
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
        val prices = MutableList(n) { lines[idx++].trim().toInt() }
        out.add(maxProfit(prices).toString())
    }
    print(out.joinToString("\n"))
}
