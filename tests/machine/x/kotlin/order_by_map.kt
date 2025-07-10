data class Data(var a: Int, var b: Int)

val data = mutableListOf(Data(a = 1, b = 2), Data(a = 1, b = 1), Data(a = 0, b = 5))

val sorted = run {
    val __res = mutableListOf<Data>()
    for (x in data) {
        __res.add(x)
    }
    __res
}.sortedBy { mutableMapOf("a" to it.a, "b" to it.b) as Comparable<Any> }

fun main() {
    println(sorted)
}
