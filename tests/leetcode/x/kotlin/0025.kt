fun revGroups(arr: List<Int>, k: Int): List<Int> {
    val out = arr.toMutableList()
    var i = 0
    while (i + k <= out.size) {
        var l = i
        var r = i + k - 1
        while (l < r) {
            val tmp = out[l]
            out[l] = out[r]
            out[r] = tmp
            l++
            r--
        }
        i += k
    }
    return out
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    var idx = 0
    val t = lines[idx++].trim().toInt()
    val out = mutableListOf<String>()
    repeat(t) {
        val n = if (idx < lines.size) lines[idx++].trim().toInt() else 0
        val arr = MutableList(n) { if (idx < lines.size) lines[idx++].trim().toInt() else 0 }
        val k = if (idx < lines.size) lines[idx++].trim().toInt() else 1
        out.add("[" + revGroups(arr, k).joinToString(",") + "]")
    }
    print(out.joinToString("\n"))
}
