private val pairs = listOf('0' to '0', '1' to '1', '6' to '9', '8' to '8', '9' to '6')

private fun build(n: Int, m: Int): List<String> {
    if (n == 0) return listOf("")
    if (n == 1) return listOf("0", "1", "8")
    val mids = build(n - 2, m)
    val res = ArrayList<String>()
    for (mid in mids) {
        for ((a, b) in pairs) {
            if (n == m && a == '0') continue
            res.add("$a$mid$b")
        }
    }
    return res
}

private fun countRange(low: String, high: String): Int {
    var ans = 0
    for (len in low.length..high.length) {
        for (s in build(len, len)) {
            if (len == low.length && s < low) continue
            if (len == high.length && s > high) continue
            ans++
        }
    }
    return ans
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = ArrayList<String>()
    var idx = 1
    repeat(t) {
        out.add(countRange(lines[idx].trim(), lines[idx + 1].trim()).toString())
        idx += 2
    }
    print(out.joinToString("\n"))
}
