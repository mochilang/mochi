fun main() {
    var matrix: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2), mutableListOf(3, 4))
    matrix[1][0] = 5
    println(matrix[1][0])
}
