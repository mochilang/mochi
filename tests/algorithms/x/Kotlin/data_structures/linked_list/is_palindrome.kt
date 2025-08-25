import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun is_palindrome(values: MutableList<Int>): Boolean {
    var stack: MutableList<Int> = mutableListOf<Int>()
    var fast: Int = (0).toInt()
    var slow: Int = (0).toInt()
    var n: Int = (values.size).toInt()
    while ((fast < n) && ((fast + 1) < n)) {
        stack = run { val _tmp = stack.toMutableList(); _tmp.add(values[slow]!!); _tmp }
        slow = slow + 1
        fast = fast + 2
    }
    if (fast == (n - 1)) {
        slow = slow + 1
    }
    var i: Int = (stack.size - 1).toInt()
    while (slow < n) {
        if (stack[i]!! != values[slow]!!) {
            return false
        }
        i = i - 1
        slow = slow + 1
    }
    return true
}

fun user_main(): Unit {
    println(is_palindrome(mutableListOf<Int>()))
    println(is_palindrome(mutableListOf(1)))
    println(is_palindrome(mutableListOf(1, 2)))
    println(is_palindrome(mutableListOf(1, 2, 1)))
    println(is_palindrome(mutableListOf(1, 2, 2, 1)))
}

fun main() {
    user_main()
}
