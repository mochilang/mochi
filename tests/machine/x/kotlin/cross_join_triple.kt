// Generated by Mochi compiler v0.10.28 on 2025-07-18T07:10:09Z
data class Row0(var n: Int, var l: String, var b: Boolean)

data class Row1(var n: Int, var l: String, var b: Boolean)

// Code generated from cross_join_triple.mochi

val nums = mutableListOf(1, 2)

val letters = mutableListOf("A", "B")

val bools = mutableListOf(true, false)

val combos = run {
    val __res = mutableListOf<Row0>()
    for (n in nums) {
        for (l in letters) {
            for (b in bools) {
                __res.add(Row0(n = n, l = l, b = b))
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
