fun each(xs: MutableList<Int>, f: (Int) -> Any): Unit {
    for (x in xs) {
        f(x)
    }
}

fun Map(xs: MutableList<Int>, f: (Int) -> Int): MutableList<Int> {
    var r: MutableList<Int> = mutableListOf()
    for (x in xs) {
        r = run { val _tmp = r.toMutableList(); _tmp.add(f(x)); _tmp } as MutableList<Int>
    }
    return r
}

fun user_main(): Unit {
    val s: MutableList<Int> = mutableListOf(1, 2, 3, 4, 5)
    each(s, { i: Int -> println((i * i).toString()) } as (Int) -> Any)
    println(Map(s, { i: Int -> i * i } as (Int) -> Int).toString())
}

fun main() {
    user_main()
}
