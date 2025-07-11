val nums = mutableListOf(3, 1, 4)

fun main() {
    println(nums.minOrNull() ?: 0)
    println(nums.maxOrNull() ?: 0)
}
