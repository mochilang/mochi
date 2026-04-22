private fun calculate(expr: String): Int {
    var result = 0
    var number = 0
    var sign = 1
    val stack = mutableListOf<Int>()
    for (ch in expr) {
        when {
            ch in '0'..'9' -> number = number * 10 + (ch - '0')
            ch == '+' || ch == '-' -> {
                result += sign * number
                number = 0
                sign = if (ch == '+') 1 else -1
            }
            ch == '(' -> {
                stack.add(result)
                stack.add(sign)
                result = 0
                number = 0
                sign = 1
            }
            ch == ')' -> {
                result += sign * number
                number = 0
                val prevSign = stack.removeAt(stack.lastIndex)
                val prevResult = stack.removeAt(stack.lastIndex)
                result = prevResult + prevSign * result
            }
        }
    }
    return result + sign * number
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    println((0 until t).joinToString("\n") { calculate(lines[it + 1]).toString() })
}
