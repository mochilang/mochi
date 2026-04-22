import java.util.PriorityQueue
import java.util.TreeMap
import java.util.TreeSet

private fun solve(words: List<String>): String {
    val chars = TreeSet<Char>()
    for (w in words) for (c in w) chars.add(c)
    val adj = TreeMap<Char, TreeSet<Char>>()
    val indeg = TreeMap<Char, Int>()
    for (c in chars) {
        adj[c] = TreeSet()
        indeg[c] = 0
    }
    for (i in 0 until words.size - 1) {
        val a = words[i]
        val b = words[i + 1]
        val m = minOf(a.length, b.length)
        if (a.substring(0, m) == b.substring(0, m) && a.length > b.length) return ""
        for (j in 0 until m) {
            if (a[j] != b[j]) {
                if (adj[a[j]]!!.add(b[j])) indeg[b[j]] = indeg[b[j]]!! + 1
                break
            }
        }
    }
    val pq = PriorityQueue<Char>()
    for ((c, d) in indeg) if (d == 0) pq.add(c)
    val out = StringBuilder()
    while (pq.isNotEmpty()) {
        val c = pq.remove()
        out.append(c)
        for (nei in adj[c]!!) {
            indeg[nei] = indeg[nei]!! - 1
            if (indeg[nei] == 0) pq.add(nei)
        }
    }
    return if (out.length == chars.size) out.toString() else ""
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = ArrayList<String>()
    var idx = 1
    repeat(t) {
        val n = lines[idx++].trim().toInt()
        out.add(solve(lines.subList(idx, idx + n).map { it.trim() }))
        idx += n
    }
    print(out.joinToString("\n"))
}
