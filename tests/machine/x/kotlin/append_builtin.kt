fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}
val a = mutableListOf(1, 2)

fun main() {
    println(append(a, 3))
}
