import java.io.BufferedReader
import java.io.InputStreamReader

fun firstMissingPositive(nums: IntArray): Int {
    val n = nums.size
    var i = 0
    while (i < n) {
        val v = nums[i]
        if (v in 1..n && nums[v - 1] != v) {
            val tmp = nums[i]
            nums[i] = nums[v - 1]
            nums[v - 1] = tmp
        } else {
            i++
        }
    }
    for (j in 0 until n) if (nums[j] != j + 1) return j + 1
    return n + 1
}

fun main() {
    val lines = generateSequence(::readLine).map { it.trim() }.toList()
    if (lines.isEmpty() || lines[0].isEmpty()) return
    var idx = 0
    val t = lines[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) {
        val n = lines[idx++].toInt()
        val nums = IntArray(n)
        for (i in 0 until n) nums[i] = lines[idx++].toInt()
        out.add(firstMissingPositive(nums).toString())
    }
    print(out.joinToString("\n"))
}
