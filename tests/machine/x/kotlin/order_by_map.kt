val data = mutableListOf(mutableMapOf("a" to 1, "b" to 2), mutableMapOf("a" to 1, "b" to 1), mutableMapOf("a" to 0, "b" to 5))

val sorted = run {
    val __res = mutableListOf<MutableMap<String, Int>>()
    for (x in data) {
        __res.add((x as MutableMap<String, Int>))
    }
    __res
}.sortedBy { mutableMapOf("a" to (it as MutableMap<*, *>)["a"], "b" to (it as MutableMap<*, *>)["b"]) as Comparable<Any> }

fun main() {
    println(sorted)
}
