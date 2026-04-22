import java.io.BufferedReader
import java.io.InputStreamReader

class MedianFinder {
    private val data = mutableListOf<Int>()

    fun addNum(num: Int) {
        var lo = 0
        var hi = data.size
        while (lo < hi) {
            val mid = (lo + hi) / 2
            if (data[mid] < num) lo = mid + 1 else hi = mid
        }
        data.add(lo, num)
    }

    fun findMedian(): Double {
        val n = data.size
        return if (n % 2 == 1) data[n / 2].toDouble() else (data[n / 2 - 1] + data[n / 2]) / 2.0
    }
}

fun main() {
    val lines = generateSequence { readLine() }.map { it.trim() }.filter { it.isNotEmpty() }.toList()
    if (lines.isEmpty()) return
    val t = lines[0].toInt()
    var idx = 1
    val blocks = mutableListOf<String>()
    repeat(t) {
        val m = lines[idx++].toInt()
        val mf = MedianFinder()
        val out = mutableListOf<String>()
        repeat(m) {
            val parts = lines[idx++].split(Regex("\\s+"))
            if (parts[0] == "addNum") {
                mf.addNum(parts[1].toInt())
            } else {
                out.add("%.1f".format(java.util.Locale.US, mf.findMedian()))
            }
        }
        blocks.add(out.joinToString("\n"))
    }
    print(blocks.joinToString("\n\n"))
}
