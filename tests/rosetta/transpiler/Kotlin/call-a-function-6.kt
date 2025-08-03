fun bar(a: Int, b: Int, c: Int): Unit {
    println((((a.toString() + ", ") + b.toString()) + ", ") + c.toString())
}

fun user_main(): Unit {
    var args: MutableMap<String, Int> = mutableMapOf<String, Int>()
    (args)["a"] = 3
    (args)["b"] = 2
    (args)["c"] = 1
    bar((args)["a"] as Int, (args)["b"] as Int, (args)["c"] as Int)
}

fun main() {
    user_main()
}
