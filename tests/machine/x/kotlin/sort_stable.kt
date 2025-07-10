data class Item(var n: Int, var v: String)

val items = mutableListOf(Item(n = 1, v = "a"), Item(n = 1, v = "b"), Item(n = 2, v = "c"))

val result = run {
    val __res = mutableListOf<String>()
    for (i in items) {
        __res.add(i.v)
    }
    __res
}.sortedBy { it.n as Comparable<Any> }

fun main() {
    println(result)
}
