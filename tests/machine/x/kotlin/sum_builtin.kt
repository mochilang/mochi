fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}
fun main() {
    println(sum(mutableListOf(1, 2, 3)))
}
