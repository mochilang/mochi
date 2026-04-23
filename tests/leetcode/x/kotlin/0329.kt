import java.io.BufferedInputStream

private class FastScanner {
    private val input = BufferedInputStream(System.`in`)
    private val buffer = ByteArray(1 shl 16)
    private var len = 0; private var ptr = 0
    private fun readByte(): Int { if (ptr >= len) { len = input.read(buffer); ptr = 0; if (len <= 0) return -1 }; return buffer[ptr++].toInt() }
    fun nextInt(): Int { var c = readByte(); while (c <= 32 && c >= 0) c = readByte(); var v = 0; while (c > 32) { v = v * 10 + c - '0'.code; c = readByte() }; return v }
}

fun longestIncreasingPath(matrix: Array<IntArray>): Int {
    val rows = matrix.size; val cols = matrix[0].size
    val memo = Array(rows) { IntArray(cols) }
    val dirs = intArrayOf(1, 0, -1, 0, 1)
    fun dfs(r: Int, c: Int): Int {
        if (memo[r][c] != 0) return memo[r][c]
        var best = 1
        for (k in 0 until 4) {
            val nr = r + dirs[k]; val nc = c + dirs[k + 1]
            if (nr in 0 until rows && nc in 0 until cols && matrix[nr][nc] > matrix[r][c]) best = maxOf(best, 1 + dfs(nr, nc))
        }
        memo[r][c] = best
        return best
    }
    var ans = 0
    for (r in 0 until rows) for (c in 0 until cols) ans = maxOf(ans, dfs(r, c))
    return ans
}

fun main() {
    val fs = FastScanner(); val t = fs.nextInt(); val out = StringBuilder()
    repeat(t) { tc ->
        val rows = fs.nextInt(); val cols = fs.nextInt()
        val m = Array(rows) { IntArray(cols) { fs.nextInt() } }
        if (tc > 0) out.append("\n\n")
        out.append(longestIncreasingPath(m))
    }
    print(out.toString())
}
