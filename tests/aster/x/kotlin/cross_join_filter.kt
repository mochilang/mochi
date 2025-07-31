fun main() {
    val nums = mutableListOf(1, 2, 3)
    val letters = mutableListOf("A", "B")
    val pairs = run()
    println("--- Even pairs ---")
    for (p in pairs) {
        println(listOf(p["n"], p["l"]).joinToString(" "))
    }
}
