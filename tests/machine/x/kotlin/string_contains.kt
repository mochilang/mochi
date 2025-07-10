val s = "catch"

fun main() {
    println((s as MutableMap<*, *>)["contains"]("cat"))
    println((s as MutableMap<*, *>)["contains"]("dog"))
}
