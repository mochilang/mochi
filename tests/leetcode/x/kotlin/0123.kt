private fun maxProfit(prices: List<Int>): Int {
    var buy1 = -1_000_000_000
    var sell1 = 0
    var buy2 = -1_000_000_000
    var sell2 = 0
    for (p in prices) {
        buy1 = maxOf(buy1, -p)
        sell1 = maxOf(sell1, buy1 + p)
        buy2 = maxOf(buy2, sell1 - p)
        sell2 = maxOf(sell2, buy2 + p)
    }
    return sell2
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
