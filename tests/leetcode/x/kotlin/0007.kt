private fun reverseInt(x0: Int): Int {
    var x = x0
    var ans = 0
    while (x != 0) {
        val digit = x % 10
        x /= 10
        if (ans > Int.MAX_VALUE / 10 || (ans == Int.MAX_VALUE / 10 && digit > 7)) return 0
        if (ans < Int.MIN_VALUE / 10 || (ans == Int.MIN_VALUE / 10 && digit < -8)) return 0
        ans = ans * 10 + digit
    }
    return ans
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = mutableListOf<String>()
    for (i in 0 until t) {
        val x = if (i + 1 < lines.size) lines[i + 1].trim().toInt() else 0
        out.add(reverseInt(x).toString())
    }
    print(out.joinToString("\n"))
}
