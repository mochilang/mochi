fun minTotalDistance(grid: List<List<Int>>): Int {
    val rows = mutableListOf<Int>()
    val cols = mutableListOf<Int>()
    for (i in grid.indices) {
        for (j in grid[i].indices) {
            if (grid[i][j] == 1) rows.add(i)
        }
    }
    for (j in grid[0].indices) {
        for (i in grid.indices) {
            if (grid[i][j] == 1) cols.add(j)
        }
    }
    val mr = rows[rows.size / 2]
    val mc = cols[cols.size / 2]
    return rows.sumOf { kotlin.math.abs(it - mr) } + cols.sumOf { kotlin.math.abs(it - mc) }
}

fun main() {
    val data = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.map { it.toInt() }.toList()
    if (data.isEmpty()) return
    var idx = 0
    val t = data[idx++]
    val out = mutableListOf<String>()
    repeat(t) {
        val r = data[idx++]
        val c = data[idx++]
        val grid = MutableList(r) { MutableList(c) { 0 } }
        for (i in 0 until r) {
            for (j in 0 until c) {
                grid[i][j] = data[idx++]
            }
        }
        out.add(minTotalDistance(grid).toString())
    }
    print(out.joinToString("\n\n"))
}
