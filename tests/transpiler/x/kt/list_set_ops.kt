fun main() {
    println((mutableListOf(1, 2) + mutableListOf(2, 3)).distinct())
    println(mutableListOf(1, 2, 3).filter { it !in mutableListOf(2) })
    println(mutableListOf(1, 2, 3).filter { it in mutableListOf(2, 4) })
    println((mutableListOf(1, 2) + mutableListOf(2, 3)).size)
}
