fun isScramble(s1: String, s2: String): Boolean {
    val memo = HashMap<String, Boolean>()
    fun dfs(i1: Int, i2: Int, len: Int): Boolean {
        val key = "$i1,$i2,$len"
        memo[key]?.let { return it }
        val a = s1.substring(i1, i1 + len)
        val b = s2.substring(i2, i2 + len)
        if (a == b) return true.also { memo[key] = it }
        val cnt = IntArray(26)
        for (i in 0 until len) {
            cnt[a[i] - 'a']++
            cnt[b[i] - 'a']--
        }
        if (cnt.any { it != 0 }) return false.also { memo[key] = it }
        for (k in 1 until len) {
            if ((dfs(i1, i2, k) && dfs(i1 + k, i2 + k, len - k)) ||
                (dfs(i1, i2 + len - k, k) && dfs(i1 + k, i2, len - k))) {
                memo[key] = true
                return true
            }
        }
        memo[key] = false
        return false
    }
    return dfs(0, 0, s1.length)
}

fun main() {
    val lines = generateSequence { readLine() }.toList()
    if (lines.isEmpty()) return
    val t = lines[0].trim().toInt()
    val out = ArrayList<String>()
    for (i in 0 until t) out.add(if (isScramble(lines[1 + 2 * i], lines[2 + 2 * i])) "true" else "false")
    print(out.joinToString("\n"))
}
