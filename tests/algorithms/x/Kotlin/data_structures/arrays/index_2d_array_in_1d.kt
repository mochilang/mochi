fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun iterator_values(matrix: MutableList<MutableList<Int>>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    for (row in matrix) {
        for (value in row) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(value); _tmp }
        }
    }
    return result
}

fun index_2d_array_in_1d(array: MutableList<MutableList<Int>>, index: Int): Int {
    var rows: Int = (array.size).toInt()
    var cols: Int = ((array[0]!!).size).toInt()
    if ((rows == 0) || (cols == 0)) {
        panic("no items in array")
    }
    if ((index < 0) || (index >= (rows * cols))) {
        panic("index out of range")
    }
    return (((array[((index / cols).toInt())]!!) as MutableList<Int>))[Math.floorMod(index, cols)]!!
}

fun main() {
    println(iterator_values(mutableListOf(mutableListOf(5), mutableListOf(0 - 523), mutableListOf(0 - 1), mutableListOf(34), mutableListOf(0))).toString())
    println(iterator_values(mutableListOf(mutableListOf(5, 0 - 523, 0 - 1), mutableListOf(34, 0))).toString())
    println(index_2d_array_in_1d(mutableListOf(mutableListOf(0, 1, 2, 3), mutableListOf(4, 5, 6, 7), mutableListOf(8, 9, 10, 11)), 5).toString())
}
