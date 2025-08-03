fun cart2(a: MutableList<Int>, b: MutableList<Int>): MutableList<MutableList<Int>> {
    var p: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (x in a) {
        for (y in b) {
            p = run { val _tmp = p.toMutableList(); _tmp.add(mutableListOf(x, y)); _tmp }
        }
    }
    return p
}

fun llStr(lst: MutableList<MutableList<Int>>): String {
    var s: String = "["
    var i: Int = 0
    while (i < lst.size) {
        var row: MutableList<Int> = lst[i]!!
        s = s + "["
        var j: Int = 0
        while (j < row.size) {
            s = s + (row[j]!!).toString()
            if (j < (row.size - 1)) {
                s = s + " "
            }
            j = j + 1
        }
        s = s + "]"
        if (i < (lst.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun user_main(): Unit {
    println(llStr(cart2(mutableListOf(1, 2), mutableListOf(3, 4))))
    println(llStr(cart2(mutableListOf(3, 4), mutableListOf(1, 2))))
    println(llStr(cart2(mutableListOf(1, 2), mutableListOf<Int>())))
    println(llStr(cart2(mutableListOf<Int>(), mutableListOf(1, 2))))
}

fun main() {
    user_main()
}
