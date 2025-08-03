fun f(): MutableList<Any?> {
    return mutableListOf<Any?>((0 as Any?), (0.0 as Any?))
}

fun g(a: Int, b: Double): Int {
    return 0
}

fun h(s: String, nums: MutableList<Int>): Unit {
}

fun user_main(): Unit {
    f()
    g(1, 2.0)
    var res: MutableList<Any?> = f()
    g(((res[0] as Any?) as Int), ((res[1] as Any?) as Double))
    g(g(1, 2.0), 3.0)
}

fun main() {
    user_main()
}
