fun show(xs: MutableList<Int>): String {
    var s: String = ""
    var i: Int = 0
    while (i < xs.size) {
        s = s + (xs[i]!!).toString()
        if (i < (xs.size - 1)) {
            s = s + " "
        }
        i = i + 1
    }
    return s
}

fun gen(init: MutableList<Int>, n: Int): MutableList<Int> {
    var b: MutableList<Int> = init
    var res: MutableList<Int> = mutableListOf<Int>()
    var sum: Int = 0
    for (x in b) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(x); _tmp } as MutableList<Int>
        sum = sum + x
    }
    while (res.size < n) {
        var next: Int = sum
        res = run { val _tmp = res.toMutableList(); _tmp.add(next.toInt()); _tmp } as MutableList<Int>
        sum = (sum + next) - b[0]!!
        b = run { val _tmp = b.subList(1, b.size).toMutableList(); _tmp.add(next.toInt()); _tmp } as MutableList<Int>
    }
    return res
}

fun user_main(): Unit {
    var n: Int = 10
    println(" Fibonacci: " + show(gen(mutableListOf(1, 1), n)))
    println("Tribonacci: " + show(gen(mutableListOf(1, 1, 2), n)))
    println("Tetranacci: " + show(gen(mutableListOf(1, 1, 2, 4), n)))
    println("     Lucas: " + show(gen(mutableListOf(2, 1), n)))
}

fun main() {
    user_main()
}
