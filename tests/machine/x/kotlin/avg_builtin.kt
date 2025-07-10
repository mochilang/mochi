fun avg(list: List<Any?>): Double {
    if (list.isEmpty()) return 0.0
    var s = 0.0
    for (n in list) s += toDouble(n)
    return s / list.size
}
fun main() {
    println(avg(mutableListOf(1, 2, 3)))
}
