fun ladders(begin: String, end: String, words: List<String>): List<List<String>> {
    val wordSet = words.toHashSet()
    if (!wordSet.contains(end)) return emptyList()
    val parents = mutableMapOf<String, MutableList<String>>()
    var level = mutableSetOf(begin)
    val visited = mutableSetOf(begin)
    var found = false
    while (level.isNotEmpty() && !found) {
        val next = mutableSetOf<String>()
        for (word in level.sorted()) {
            val arr = word.toCharArray()
            for (i in arr.indices) {
                val orig = arr[i]
                for (c in 'a'..'z') {
                    if (c == orig) continue
                    arr[i] = c
                    val nw = String(arr)
                    if (!wordSet.contains(nw) || visited.contains(nw)) continue
                    next.add(nw)
                    parents.getOrPut(nw) { mutableListOf() }.add(word)
                    if (nw == end) found = true
                }
                arr[i] = orig
            }
        }
        visited.addAll(next)
        level = next
    }
    if (!found) return emptyList()
    val out = mutableListOf<List<String>>()
    val path = mutableListOf(end)
    fun dfs(word: String) {
        if (word == begin) {
            out.add(path.reversed())
            return
        }
        for (p in parents[word]!!.sorted()) {
            path.add(p)
            dfs(p)
            path.removeAt(path.size - 1)
        }
    }
    dfs(end)
    return out.sortedBy { it.joinToString("->") }
}

fun fmt(paths: List<List<String>>): String {
    val lines = mutableListOf(paths.size.toString())
    for (p in paths) lines.add(p.joinToString("->"))
    return lines.joinToString("\n")
}

fun main() {
    val lines = generateSequence(::readLine).toList()
    if (lines.isEmpty()) return
    val tc = lines[0].toInt()
    var idx = 1
    val out = mutableListOf<String>()
    repeat(tc) {
        val begin = lines[idx++]
        val end = lines[idx++]
        val n = lines[idx++].toInt()
        val words = mutableListOf<String>()
        repeat(n) { words.add(lines[idx++]) }
        out.add(fmt(ladders(begin, end, words)))
    }
    print(out.joinToString("\n\n"))
}
