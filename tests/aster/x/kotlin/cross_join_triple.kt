fun main() {
    val nums = mutableListOf(1, 2)
    val letters = mutableListOf("A", "B")
    val bools = mutableListOf(true, false)
    val combos = run()
    println("--- Cross Join of three lists ---")
    for (c in combos) {
        println(listOf(c["n"], c["l"], c["b"]).joinToString(" "))
    }
}
