import java.math.BigInteger

var arr1: MutableList<Int> = mutableListOf(0, 1, 2, 8, 13, 17, 19, 32, 42)
fun binary_search(arr: MutableList<Int>, item: Int): Boolean {
    var low: Int = (0).toInt()
    var high: BigInteger = ((arr.size - 1).toBigInteger())
    while ((low).toBigInteger().compareTo((high)) <= 0) {
        var mid = ((low).toBigInteger().add((high))).divide((2).toBigInteger())
        var _val: Int = (arr[(mid).toInt()]!!).toInt()
        if (_val == item) {
            return true
        }
        if (item < _val) {
            high = mid.subtract((1).toBigInteger())
        } else {
            low = ((mid.add((1).toBigInteger())).toInt())
        }
    }
    return false
}

fun main() {
    println(binary_search(arr1, 3))
    println(binary_search(arr1, 13))
    var arr2: MutableList<Int> = mutableListOf(4, 4, 5, 6, 7)
    println(binary_search(arr2, 4))
    println(binary_search(arr2, 0 - 10))
    var arr3: MutableList<Int> = mutableListOf(0 - 18, 2)
    println(binary_search(arr3, 0 - 18))
    var arr4: MutableList<Int> = mutableListOf(5)
    println(binary_search(arr4, 5))
    var arr5: MutableList<Int> = mutableListOf<Int>()
    println(binary_search(arr5, 1))
}
