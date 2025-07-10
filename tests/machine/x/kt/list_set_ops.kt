fun main() {
    val union = (setOf(1, 2) union setOf(2, 3)).toList()
    println(union)
    val except = listOf(1, 2, 3).filter { it !in setOf(2) }
    println(except)
    val intersect = listOf(1, 2, 3).filter { it in setOf(2, 4) }
    println(intersect)
    val unionAllLen = listOf(1, 2) + listOf(2, 3)
    println(unionAllLen.size)
}
