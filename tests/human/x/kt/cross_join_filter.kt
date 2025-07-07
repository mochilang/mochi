fun main() {
    val nums = listOf(1, 2, 3)
    val letters = listOf("A", "B")
    val pairs = mutableListOf<Pair<Int, String>>()
    for (n in nums) {
        if (n % 2 == 0) {
            for (l in letters) {
                pairs.add(Pair(n, l))
            }
        }
    }
    println("--- Even pairs ---")
    for (p in pairs) {
        println("${p.first} ${p.second}")
    }
}
