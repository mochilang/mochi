fun <T> union(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = a.toMutableList()
    for (x in b) if (!res.contains(x)) res.add(x)
    return res
}

fun <T> except(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (!b.contains(x)) res.add(x)
    return res
}

fun <T> intersect(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (b.contains(x)) res.add(x)
    return res
}
fun main() {
    println(union(mutableListOf(1, 2).toMutableList(), mutableListOf(2, 3).toMutableList()))
    println(except(mutableListOf(1, 2, 3).toMutableList(), mutableListOf(2).toMutableList()))
    println(intersect(mutableListOf(1, 2, 3).toMutableList(), mutableListOf(2, 4).toMutableList()))
    println(mutableListOf(1, 2).toMutableList().apply { addAll(mutableListOf(2, 3)) }.size)
}
