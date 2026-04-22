fun solve(k: Int, prices: List<Int>): Int {
    val n = prices.size
    if (k >= n / 2) {
        var best = 0
        for (i in 1 until n) if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1]
        return best
    }
    val negInf = -(1L shl 60)
    val buy = MutableList(k + 1) { negInf }
    val sell = MutableList(k + 1) { 0L }
    for (price in prices) {
        for (t in 1..k) {
            buy[t] = maxOf(buy[t], sell[t - 1] - price)
            sell[t] = maxOf(sell[t], buy[t] + price)
        }
    }
    return sell[k].toInt()
}

fun main() {
    val toks = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) {
        val k = toks[idx++].toInt()
        val n = toks[idx++].toInt()
        val prices = ArrayList<Int>()
        repeat(n) { prices.add(toks[idx++].toInt()) }
        out.add(solve(k, prices).toString())
    }
    print(out.joinToString("\n"))
}
