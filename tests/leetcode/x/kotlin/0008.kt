private fun myAtoi(s: String): Int {
    var i = 0
    while (i < s.length && s[i] == ' ') i++
    var sign = 1
    if (i < s.length && (s[i] == '+' || s[i] == '-')) {
        if (s[i] == '-') sign = -1
        i++
    }
    var ans = 0
    val limit = if (sign > 0) 7 else 8
    while (i < s.length && s[i].isDigit()) {
        val digit = s[i] - '0'
        if (ans > 214748364 || (ans == 214748364 && digit > limit)) return if (sign > 0) Int.MAX_VALUE else Int.MIN_VALUE
        ans = ans * 10 + digit
        i++
    }
    return sign * ans
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = mutableListOf<String>()
    for (i in 0 until t) out.add(myAtoi(if (i + 1 < lines.size) lines[i + 1] else "").toString())
    print(out.joinToString("\n"))
}
