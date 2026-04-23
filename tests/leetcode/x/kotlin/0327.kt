import java.io.BufferedInputStream

private class FastScanner {
    private val input = BufferedInputStream(System.`in`)
    private val buffer = ByteArray(1 shl 16)
    private var len = 0
    private var ptr = 0
    private fun readByte(): Int {
        if (ptr >= len) {
            len = input.read(buffer)
            ptr = 0
            if (len <= 0) return -1
        }
        return buffer[ptr++].toInt()
    }
    fun nextInt(): Int {
        var c = readByte()
        while (c <= 32 && c >= 0) c = readByte()
        var sign = 1
        if (c == '-'.code) { sign = -1; c = readByte() }
        var v = 0
        while (c > 32) { v = v * 10 + c - '0'.code; c = readByte() }
        return v * sign
    }
}

fun countRangeSum(nums: IntArray, lower: Int, upper: Int): Int {
    val pref = LongArray(nums.size + 1)
    for (i in nums.indices) pref[i + 1] = pref[i] + nums[i].toLong()
    val tmp = LongArray(pref.size)
    fun sort(lo: Int, hi: Int): Int {
        if (hi - lo <= 1) return 0
        val mid = (lo + hi) / 2
        var ans = sort(lo, mid) + sort(mid, hi)
        var left = lo
        var right = lo
        for (r in mid until hi) {
            while (left < mid && pref[left] < pref[r] - upper) left++
            while (right < mid && pref[right] <= pref[r] - lower) right++
            ans += right - left
        }
        var i = lo; var j = mid; var k = lo
        while (i < mid && j < hi) tmp[k++] = if (pref[i] <= pref[j]) pref[i++] else pref[j++]
        while (i < mid) tmp[k++] = pref[i++]
        while (j < hi) tmp[k++] = pref[j++]
        for (p in lo until hi) pref[p] = tmp[p]
        return ans
    }
    return sort(0, pref.size)
}

fun main() {
    val fs = FastScanner()
    val t = fs.nextInt()
    val out = StringBuilder()
    repeat(t) { tc ->
        val n = fs.nextInt()
        val nums = IntArray(n) { fs.nextInt() }
        val lower = fs.nextInt()
        val upper = fs.nextInt()
        if (tc > 0) out.append("\n\n")
        out.append(countRangeSum(nums, lower, upper))
    }
    print(out.toString())
}
