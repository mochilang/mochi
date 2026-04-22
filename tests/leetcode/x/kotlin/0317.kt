fun shortestDistance(grid: List<List<Int>>): Int {
    val rows = grid.size
    val cols = grid[0].size
    val dist = Array(rows) { IntArray(cols) }
    val reach = Array(rows) { IntArray(cols) }
    var buildings = 0
    for (sr in 0 until rows) {
        for (sc in 0 until cols) {
            if (grid[sr][sc] != 1) continue
            buildings++
            val seen = Array(rows) { BooleanArray(cols) }
            val q = ArrayDeque<IntArray>()
            q.add(intArrayOf(sr, sc, 0))
            seen[sr][sc] = true
            while (q.isNotEmpty()) {
                val (r, c, d) = q.removeFirst()
                for ((dr, dc) in listOf(1 to 0, -1 to 0, 0 to 1, 0 to -1)) {
                    val nr = r + dr
                    val nc = c + dc
                    if (nr in 0 until rows && nc in 0 until cols && !seen[nr][nc]) {
                        seen[nr][nc] = true
                        if (grid[nr][nc] == 0) {
                            dist[nr][nc] += d + 1
                            reach[nr][nc] += 1
                            q.add(intArrayOf(nr, nc, d + 1))
                        }
                    }
                }
            }
        }
    }
    var ans = -1
    for (r in 0 until rows) {
        for (c in 0 until cols) {
            if (grid[r][c] == 0 && reach[r][c] == buildings) {
                if (ans == -1 || dist[r][c] < ans) ans = dist[r][c]
            }
        }
    }
    return ans
}

fun main() {
    val data = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.map { it.toInt() }.toList()
    if (data.isEmpty()) return
    var pos = 0
    val t = data[pos++]
    val blocks = mutableListOf<String>()
    repeat(t) {
        val rows = data[pos++]
        val cols = data[pos++]
        val grid = MutableList(rows) { MutableList(cols) { 0 } }
        for (i in 0 until rows) {
            for (j in 0 until cols) {
                grid[i][j] = data[pos++]
            }
        }
        blocks.add(shortestDistance(grid).toString())
    }
    print(blocks.joinToString("\n\n"))
}
