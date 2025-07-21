fun main() {
    val nums = mutableListOf(1, 2, 3)
    val letters = mutableListOf("A", "B")
    val pairs = run {
    val _res = mutableListOf<MutableMap<String, Any>>()
    for (n in nums) {
        for (l in letters) {
            if ((n % 2) == 0) {
                _res.add(mutableMapOf("n" to n, "l" to l) as MutableMap<String, Any>)
            }
        }
    }
    _res
}
    println("--- Even pairs ---")
    for (p in pairs) {
        println(listOf(p["n"]!!, p["l"]!!).joinToString(" "))
    }
}
