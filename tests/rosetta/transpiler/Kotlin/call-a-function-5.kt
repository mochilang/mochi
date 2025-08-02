fun doIt(p: MutableMap<String, Int>): Int {
    var b: Int = 0
    if ("b" in p) {
        b = (p)["b"] as Int
    }
    return ((p)["a"] as Int + b) + (p)["c"] as Int
}

fun user_main(): Unit {
    var p: MutableMap<String, Int> = mutableMapOf<String, Int>()
    (p)["a"] = 1
    (p)["c"] = 9
    println(doIt(p).toString())
}

fun main() {
    user_main()
}
