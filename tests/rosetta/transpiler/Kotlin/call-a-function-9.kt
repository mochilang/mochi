fun f(): MutableList<Any?> {
    return mutableListOf<Any?>((0 as Any?), (0.0 as Any?))
}

fun g(a: Int, b: Double): Int {
    return 0
}

fun h(s: String, nums: MutableList<Int>): Unit {
}

fun user_main(): Unit {
    var ab: MutableList<Any?> = f()
    var a: Any? = ab[0] as Any?
    var b: Any? = ab[1] as Any?
    var cb: Any? = (f())[1] as Any?
    var d: Int = g((a as Int), (cb as Double))
    var e: Int = g(d, (b as Double))
    var i: Int = g(d, 2.0)
    var list: MutableList<Int> = mutableListOf<Int>()
    list = run { val _tmp = list.toMutableList(); _tmp.add((a as Int)); _tmp }
    list = run { val _tmp = list.toMutableList(); _tmp.add(d); _tmp }
    list = run { val _tmp = list.toMutableList(); _tmp.add(e); _tmp }
    list = run { val _tmp = list.toMutableList(); _tmp.add(i); _tmp }
    i = list.size
}

fun main() {
    user_main()
}
