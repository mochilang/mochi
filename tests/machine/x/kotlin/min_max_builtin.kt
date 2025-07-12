// Code generated from tests/vm/valid/min_max_builtin.mochi

val nums = mutableListOf(3, 1, 4)

fun main() {
    println(nums.min() ?: 0)
    println(nums.max() ?: 0)
}
