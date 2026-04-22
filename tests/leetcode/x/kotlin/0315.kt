fun sortCounts(nums: IntArray, idx: IntArray, tmp: IntArray, counts: IntArray, lo: Int, hi: Int) {
    if (hi - lo <= 1) return
    val mid = (lo + hi) / 2
    sortCounts(nums, idx, tmp, counts, lo, mid)
    sortCounts(nums, idx, tmp, counts, mid, hi)
    var i = lo
    var j = mid
    var k = lo
    var moved = 0
    while (i < mid && j < hi) {
        if (nums[idx[j]] < nums[idx[i]]) {
            tmp[k++] = idx[j++]
            moved++
        } else {
            counts[idx[i]] += moved
            tmp[k++] = idx[i++]
        }
    }
    while (i < mid) {
        counts[idx[i]] += moved
        tmp[k++] = idx[i++]
    }
    while (j < hi) tmp[k++] = idx[j++]
    for (p in lo until hi) idx[p] = tmp[p]
}

fun countSmaller(nums: IntArray): IntArray {
    val n = nums.size
    val counts = IntArray(n)
    val idx = IntArray(n) { it }
    val tmp = IntArray(n)
    sortCounts(nums, idx, tmp, counts, 0, n)
    return counts
}

fun main() {
    val data = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.map { it.toInt() }.toList()
    if (data.isEmpty()) return
    var pos = 0
    val t = data[pos++]
    val blocks = mutableListOf<String>()
    repeat(t) {
        val n = data[pos++]
        val nums = IntArray(n) { data[pos + it] }
        pos += n
        blocks.add("[" + countSmaller(nums).joinToString(",") + "]")
    }
    print(blocks.joinToString("\n\n"))
}
