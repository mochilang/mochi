val nums = mutableListOf(1, 2, 3)

val result = run {
    val __res = mutableListOf<Any?>()
    for (n in nums) {
        if (n > 1) {
            __res.add(n.sum())
        }
    }
    __res
}

fun main() {
    println(result)
}
