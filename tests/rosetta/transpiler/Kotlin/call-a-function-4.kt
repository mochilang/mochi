fun gifEncode(out: Any?, img: Any?, opts: MutableMap<String, Int>): Unit {
}

fun user_main(): Unit {
    var opts: MutableMap<String, Int> = mutableMapOf<String, Int>()
    (opts)["NumColors"] = 16
    gifEncode((null as Any?), (null as Any?), opts)
}

fun main() {
    user_main()
}
