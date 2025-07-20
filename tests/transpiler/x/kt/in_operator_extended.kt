fun main() {
    val xs: MutableList<Int> = mutableListOf(1, 2, 3)
    val ys: MutableList<Int> = run {
    val _res = mutableListOf<Any>()
    for (x in xs) {
        if ((x % 2) == 1) {
            _res.add(x)
        }
    }
    _res
}
    println(1 in ys)
    println(2 in ys)
    val m: MutableMap<String, Int> = mutableMapOf("a" to 1)
    println("a" in m)
    println("b" in m)
    val s: String = "hello"
    println("ell" in s)
    println("foo" in s)
}
