import java.util.Scanner

class Solution {
    fun removeDuplicates(nums: IntArray): Int {
        if (nums.isEmpty()) return 0
        var k = 1
        for (i in 1 until nums.size) {
            if (nums[i] != nums[k - 1]) {
                nums[k] = nums[i]
                k++
            }
        }
        return k
    }
}

fun main(args: Array<String>) {
    val sc = Scanner(System.`in`)
    if (!sc.hasNextInt()) return
    val t = sc.nextInt()
    val sol = Solution()
    repeat(t) {
        val n = sc.nextInt()
        val nums = IntArray(n)
        for (i in 0 until n) {
            nums[i] = sc.nextInt()
        }
        val k = sol.removeDuplicates(nums)
        val res = mutableListOf<String>()
        for (i in 0 until k) {
            res.add(nums[i].toString())
        }
        println(res.joinToString(" "))
    }
}
