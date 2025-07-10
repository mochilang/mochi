val nums = mutableListOf(1, 2)

val letters = mutableListOf("A", "B")

val bools = mutableListOf(true, false)

val combos = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (n in nums) {
        for (l in letters) {
            for (b in bools) {
                __res.add((mutableMapOf("n" to n, "l" to l, "b" to b) as MutableMap<Any?, Any?>))
            }
        }
    }
    __res
}

fun main() {
    println("--- Cross Join of three lists ---")
    for (c in combos) {
        println(listOf((c as MutableMap<*, *>)["n"], (c as MutableMap<*, *>)["l"], (c as MutableMap<*, *>)["b"]).joinToString(" "))
    }
}
