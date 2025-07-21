fun main() {
    val nums = mutableListOf(1, 2)
    val letters = mutableListOf("A", "B")
    val bools = mutableListOf(true, false)
    val combos = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (n in nums) {
        for (l in letters) {
            for (b in bools) {
                _res.add(mutableMapOf("n" to n, "l" to l, "b" to b))
            }
        }
    }
    _res
}
    println("--- Cross Join of three lists ---")
    for (c in combos) {
        println(listOf(c["n"], c["l"], c["b"]).joinToString(" "))
    }
}
