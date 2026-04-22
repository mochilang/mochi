fun findSet(x0: Int, parent: MutableMap<Int, Int>): Int {
    var x = x0
    while (parent[x]!! != x) {
        parent[x] = parent[parent[x]!!]!!
        x = parent[x]!!
    }
    return x
}

fun unionSet(a: Int, b: Int, parent: MutableMap<Int, Int>, rank: MutableMap<Int, Int>): Boolean {
    var ra = findSet(a, parent)
    var rb = findSet(b, parent)
    if (ra == rb) return false
    if (rank[ra]!! < rank[rb]!!) {
        val t = ra
        ra = rb
        rb = t
    }
    parent[rb] = ra
    if (rank[ra] == rank[rb]) rank[ra] = rank[ra]!! + 1
    return true
}

fun solve(m: Int, n: Int, positions: List<Pair<Int, Int>>): List<Int> {
    val parent = mutableMapOf<Int, Int>()
    val rank = mutableMapOf<Int, Int>()
    val ans = mutableListOf<Int>()
    var count = 0
    for ((r, c) in positions) {
        val idx = r * n + c
        if (parent.containsKey(idx)) {
            ans.add(count)
            continue
        }
        parent[idx] = idx
        rank[idx] = 0
        count++
        for ((dr, dc) in listOf(1 to 0, -1 to 0, 0 to 1, 0 to -1)) {
            val nr = r + dr
            val nc = c + dc
            if (nr in 0 until m && nc in 0 until n) {
                val nei = nr * n + nc
                if (parent.containsKey(nei) && unionSet(idx, nei, parent, rank)) count--
            }
        }
        ans.add(count)
    }
    return ans
}

fun main() {
    val data = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.map { it.toInt() }.toList()
    if (data.isEmpty()) return
    var idx = 0
    val t = data[idx++]
    val blocks = mutableListOf<String>()
    repeat(t) {
        val m = data[idx++]
        val n = data[idx++]
        val k = data[idx++]
        val positions = mutableListOf<Pair<Int, Int>>()
        repeat(k) { positions.add(data[idx++] to data[idx++]) }
        blocks.add("[" + solve(m, n, positions).joinToString(",") + "]")
    }
    print(blocks.joinToString("\n\n"))
}
