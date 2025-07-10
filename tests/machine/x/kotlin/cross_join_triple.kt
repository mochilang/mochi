data class Combo(var n: Any?, var l: Any?, var b: Any?)

val nums = mutableListOf(1, 2)

val letters = mutableListOf("A", "B")

val bools = mutableListOf(true, false)

val combos = run {
    val __res = mutableListOf<Combo>()
    for (n in nums) {
        for (l in letters) {
            for (b in bools) {
                __res.add(Combo(n = n, l = l, b = b))
            }
        }
    }
    __res
}

fun main() {
    println("--- Cross Join of three lists ---")
    for (c in combos) {
        println(listOf(c.n, c.l, c.b).joinToString(" "))
    }
}
