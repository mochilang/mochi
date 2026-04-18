fun getPermutation(n: Int, kInput: Int): String {
    val digits = (1..n).map { it.toString() }.toMutableList()
    val fact = IntArray(n + 1)
    fact[0] = 1
    for (i in 1..n) fact[i] = fact[i - 1] * i
    var k = kInput - 1
    val out = StringBuilder()
    for (rem in n downTo 1) {
        val block = fact[rem - 1]
        val idx = k / block
        k %= block
        out.append(digits.removeAt(idx))
    }
    return out.toString()
}

fun main() {
    val lines = generateSequence(::readLine).map { it.trim() }.toList()
    if (lines.isEmpty() || lines[0].isEmpty()) return
    var idx = 0
    val t = lines[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) {
        val n = lines[idx++].toInt()
        val k = lines[idx++].toInt()
        out.add(getPermutation(n, k))
    }
    print(out.joinToString("\n"))
}
