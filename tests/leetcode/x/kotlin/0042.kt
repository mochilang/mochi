fun trap(height: IntArray): Int {
    var left = 0
    var right = height.size - 1
    var leftMax = 0
    var rightMax = 0
    var water = 0
    while (left <= right) {
        if (leftMax <= rightMax) {
            if (height[left] < leftMax) water += leftMax - height[left] else leftMax = height[left]
            left++
        } else {
            if (height[right] < rightMax) water += rightMax - height[right] else rightMax = height[right]
            right--
        }
    }
    return water
}

fun main() {
    val lines = generateSequence(::readLine).map { it.trim() }.toList()
    if (lines.isEmpty() || lines[0].isEmpty()) return
    var idx = 0
    val t = lines[idx++].toInt()
    val out = ArrayList<String>()
    repeat(t) {
        val n = lines[idx++].toInt()
        val arr = IntArray(n)
        for (i in 0 until n) arr[i] = lines[idx++].toInt()
        out.add(trap(arr).toString())
    }
    print(out.joinToString("\n"))
}
