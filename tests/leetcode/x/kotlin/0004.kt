fun median(a: List<Int>, b: List<Int>): Double {
    val m = mutableListOf<Int>()
    var i = 0
    var j = 0
    while (i < a.size && j < b.size) {
        if (a[i] <= b[j]) m.add(a[i++]) else m.add(b[j++])
    }
    while (i < a.size) m.add(a[i++])
    while (j < b.size) m.add(b[j++])
    return if (m.size % 2 == 1) m[m.size / 2].toDouble() else (m[m.size / 2 - 1] + m[m.size / 2]) / 2.0
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(t) {
        val n = lines[idx++].trim().toInt()
        val a = MutableList(n) { lines[idx++].trim().toInt() }
        val m = lines[idx++].trim().toInt()
        val b = MutableList(m) { lines[idx++].trim().toInt() }
        out.add(String.format(java.util.Locale.US, "%.1f", median(a, b)))
    }
    print(out.joinToString("\n"))
}
