fun isNumber(s: String): Boolean {
    var seenDigit = false
    var seenDot = false
    var seenExp = false
    var digitAfterExp = true

    for (i in s.indices) {
        val ch = s[i]
        when {
            ch in '0'..'9' -> {
                seenDigit = true
                if (seenExp) digitAfterExp = true
            }
            ch == '+' || ch == '-' -> {
                if (i != 0 && s[i - 1] != 'e' && s[i - 1] != 'E') return false
            }
            ch == '.' -> {
                if (seenDot || seenExp) return false
                seenDot = true
            }
            ch == 'e' || ch == 'E' -> {
                if (seenExp || !seenDigit) return false
                seenExp = true
                digitAfterExp = false
            }
            else -> return false
        }
    }

    return seenDigit && digitAfterExp
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isNotEmpty()) {
        val t = lines[0].trim().toInt()
        val out = ArrayList<String>()
        repeat(t) { i ->
            out.add(if (isNumber(lines[i + 1])) "true" else "false")
        }
        print(out.joinToString("\n"))
    }
}
