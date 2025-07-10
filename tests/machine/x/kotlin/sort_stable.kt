val items = mutableListOf(mutableMapOf("n" to 1, "v" to "a"), mutableMapOf("n" to 1, "v" to "b"), mutableMapOf("n" to 2, "v" to "c"))

val result = run {
    val __res = mutableListOf<Any?>()
    for (i in items) {
        __res.add((i as MutableMap<*, *>)["v"])
    }
    __res
}.sortedBy { (it as MutableMap<*, *>)["n"] as Comparable<Any> }

fun main() {
    println(result)
}
