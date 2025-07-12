// Code generated from tests/vm/valid/list_nested_assign.mochi

var matrix = mutableListOf(mutableListOf(1, 2), mutableListOf(3, 4))

fun main() {
    matrix[1]!![0] = 5
    println((matrix[1] as MutableList<Int>)[0])
}
