fun main() {
    val nums = listOf(1, 2)
    val letters = listOf("A", "B")
    val bools = listOf(true, false)
    val combos = mutableListOf<Triple<Int, String, Boolean>>()
    for (n in nums) {
        for (l in letters) {
            for (b in bools) {
                combos.add(Triple(n, l, b))
            }
        }
    }
    println("--- Cross Join of three lists ---")
    for (c in combos) {
        println("${c.first} ${c.second} ${c.third}")
    }
}
