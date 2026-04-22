private fun countDigitOne(n: Long): Long {
    var total = 0L
    var m = 1L
    while (m <= n) {
        val high = n / (m * 10)
        val cur = (n / m) % 10
        val low = n % m
        total += when {
            cur == 0L -> high * m
            cur == 1L -> high * m + low + 1
            else -> (high + 1) * m
        }
        m *= 10
    }
    return total
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    println((0 until t).joinToString("\n") { countDigitOne(lines[it + 1].trim().toLong()).toString() })
}
