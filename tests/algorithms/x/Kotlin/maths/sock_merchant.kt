import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun sock_merchant(colors: MutableList<Int>): Int {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < colors.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(colors[i]!!); _tmp }
        i = i + 1
    }
    var n: Int = (arr.size).toInt()
    var a: Int = (0).toInt()
    while (a < n) {
        var min_idx: Int = (a).toInt()
        var b: Int = (a + 1).toInt()
        while (b < n) {
            if (arr[b]!! < arr[min_idx]!!) {
                min_idx = b
            }
            b = b + 1
        }
        var temp: Int = (arr[a]!!).toInt()
        _listSet(arr, a, arr[min_idx]!!)
        _listSet(arr, min_idx, temp)
        a = a + 1
    }
    var pairs: Int = (0).toInt()
    i = 0
    while (i < n) {
        var count: Int = (1).toInt()
        while (((i + 1) < n) && (arr[i]!! == arr[i + 1]!!)) {
            count = count + 1
            i = i + 1
        }
        pairs = pairs + (count / 2)
        i = i + 1
    }
    return pairs
}

fun test_sock_merchant(): Unit {
    var example1: MutableList<Int> = mutableListOf(10, 20, 20, 10, 10, 30, 50, 10, 20)
    if (sock_merchant(example1) != 3) {
        panic("example1 failed")
    }
    var example2: MutableList<Int> = mutableListOf(1, 1, 3, 3)
    if (sock_merchant(example2) != 2) {
        panic("example2 failed")
    }
}

fun user_main(): Unit {
    test_sock_merchant()
    var example1: MutableList<Int> = mutableListOf(10, 20, 20, 10, 10, 30, 50, 10, 20)
    println(_numToStr(sock_merchant(example1)))
    var example2: MutableList<Int> = mutableListOf(1, 1, 3, 3)
    println(_numToStr(sock_merchant(example2)))
}

fun main() {
    user_main()
}
