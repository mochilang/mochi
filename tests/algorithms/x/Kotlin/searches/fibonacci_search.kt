import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var example1: MutableList<Int> = mutableListOf(4, 5, 6, 7)
var example2: MutableList<Int> = mutableListOf(0 - 18, 2)
var example3: MutableList<Int> = mutableListOf(0, 5, 10, 15, 20, 25, 30)
fun fibonacci(k: Int): Int {
    if (k < 0) {
        panic("k must be >= 0")
    }
    var a: Int = (0).toInt()
    var b: Int = (1).toInt()
    var i: Int = (0).toInt()
    while (i < k) {
        var tmp: Int = (a + b).toInt()
        a = b
        b = tmp
        i = i + 1
    }
    return a
}

fun min_int(a: Int, b: Int): Int {
    if (a < b) {
        return a
    } else {
        return b
    }
}

fun fibonacci_search(arr: MutableList<Int>, _val: Int): Int {
    var n: Int = (arr.size).toInt()
    var m: Int = (0).toInt()
    while (fibonacci(m) < n) {
        m = m + 1
    }
    var offset: Int = (0).toInt()
    while (m > 0) {
        var i: Int = (min_int(offset + fibonacci(m - 1), n - 1)).toInt()
        var item: Int = (arr[i]!!).toInt()
        if (item == _val) {
            return i
        } else {
            if (_val < item) {
                m = m - 1
            } else {
                offset = offset + fibonacci(m - 1)
                m = m - 2
            }
        }
    }
    return 0 - 1
}

fun main() {
    println(fibonacci_search(example1, 4).toString())
    println(fibonacci_search(example1, 0 - 10).toString())
    println(fibonacci_search(example2, 0 - 18).toString())
    println(fibonacci_search(example3, 15).toString())
    println(fibonacci_search(example3, 17).toString())
}
