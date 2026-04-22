private val less20 = arrayOf("", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
    "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen")
private val tens = arrayOf("", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety")
private val thousands = arrayOf("", "Thousand", "Million", "Billion")

private fun helper(n: Int): String =
    when {
        n == 0 -> ""
        n < 20 -> less20[n]
        n < 100 -> tens[n / 10] + if (n % 10 == 0) "" else " ${helper(n % 10)}"
        else -> less20[n / 100] + " Hundred" + if (n % 100 == 0) "" else " ${helper(n % 100)}"
    }

private fun solve(num0: Int): String {
    if (num0 == 0) return "Zero"
    var num = num0
    val parts = ArrayList<String>()
    var idx = 0
    while (num > 0) {
        val chunk = num % 1000
        if (chunk != 0) {
            var words = helper(chunk)
            if (thousands[idx].isNotEmpty()) words += " ${thousands[idx]}"
            parts.add(0, words)
        }
        num /= 1000
        idx++
    }
    return parts.joinToString(" ")
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    println((0 until t).joinToString("\n") { solve(lines[it + 1].trim().toInt()) })
}
