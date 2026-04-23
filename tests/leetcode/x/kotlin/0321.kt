fun pick(nums: IntArray, k: Int): IntArray {
    var drop = nums.size - k
    val stack = mutableListOf<Int>()
    for (x in nums) {
        while (drop > 0 && stack.isNotEmpty() && stack.last() < x) {
            stack.removeAt(stack.lastIndex)
            drop--
        }
        stack.add(x)
    }
    return stack.take(k).toIntArray()
}

fun greater(a: IntArray, i0: Int, b: IntArray, j0: Int): Boolean {
    var i = i0
    var j = j0
    while (i < a.size && j < b.size && a[i] == b[j]) {
        i++
        j++
    }
    return j == b.size || (i < a.size && a[i] > b[j])
}

fun merge(a: IntArray, b: IntArray): IntArray {
    val out = IntArray(a.size + b.size)
    var i = 0
    var j = 0
    for (p in out.indices) {
        if (greater(a, i, b, j)) out[p] = a[i++] else out[p] = b[j++]
    }
    return out
}

fun maxNumber(nums1: IntArray, nums2: IntArray, k: Int): IntArray {
    var best = IntArray(0)
    for (take in maxOf(0, k - nums2.size)..minOf(k, nums1.size)) {
        val cand = merge(pick(nums1, take), pick(nums2, k - take))
        if (greater(cand, 0, best, 0)) best = cand
    }
    return best
}

fun main() {
    val data = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.map { it.toInt() }.toList()
    if (data.isEmpty()) return
    var idx = 0
    val t = data[idx++]
    val blocks = mutableListOf<String>()
    repeat(t) {
        val n1 = data[idx++]
        val nums1 = IntArray(n1) { data[idx + it] }
        idx += n1
        val n2 = data[idx++]
        val nums2 = IntArray(n2) { data[idx + it] }
        idx += n2
        val k = data[idx++]
        blocks.add("[" + maxNumber(nums1, nums2, k).joinToString(",") + "]")
    }
    print(blocks.joinToString("\n\n"))
}
