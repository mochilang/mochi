fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun hamming(n: Int): MutableList<Int> {
    if (n < 1) {
        panic("n_element should be a positive number")
    }
    var hamming_list: MutableList<Int> = mutableListOf(1)
    var i: Int = (0).toInt()
    var j: Int = (0).toInt()
    var k: Int = (0).toInt()
    var index: Int = (1).toInt()
    while (index < n) {
        while ((hamming_list[i]!! * 2) <= hamming_list[hamming_list.size - 1]!!) {
            i = i + 1
        }
        while ((hamming_list[j]!! * 3) <= hamming_list[hamming_list.size - 1]!!) {
            j = j + 1
        }
        while ((hamming_list[k]!! * 5) <= hamming_list[hamming_list.size - 1]!!) {
            k = k + 1
        }
        var m1: Int = (hamming_list[i]!! * 2).toInt()
        var m2: Int = (hamming_list[j]!! * 3).toInt()
        var m3: Int = (hamming_list[k]!! * 5).toInt()
        var next: Int = (m1).toInt()
        if (m2 < next) {
            next = m2
        }
        if (m3 < next) {
            next = m3
        }
        hamming_list = run { val _tmp = hamming_list.toMutableList(); _tmp.add(next); _tmp }
        index = index + 1
    }
    return hamming_list
}

fun main() {
    println(hamming(5))
    println(hamming(10))
    println(hamming(15))
}
