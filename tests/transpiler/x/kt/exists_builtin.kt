fun main() {
    val data: MutableList<Int> = mutableListOf(1, 2)
    val flag: Boolean = run {
    val _res = mutableListOf<Any>()
    for (x in data) {
        if (x == 1) {
            _res.add(x)
        }
    }
    _res
}.isNotEmpty()
    println(flag)
}
