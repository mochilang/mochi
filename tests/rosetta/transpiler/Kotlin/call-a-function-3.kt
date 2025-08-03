fun f(): MutableList<Any?> {
    return mutableListOf<Any?>((0 as Any?), (0.0 as Any?))
}

fun g(a: Int, b: Double): Int {
    return 0
}

fun h(s: String, nums: MutableList<Int>): Unit {
}

fun user_main(): Unit {
    h("ex1", mutableListOf<Int>())
    h("ex2", mutableListOf(1, 2))
    h("ex3", mutableListOf(1, 2, 3, 4))
    var list: MutableList<Int> = mutableListOf(1, 2, 3, 4)
    h("ex4", list)
}

fun main() {
    user_main()
}
