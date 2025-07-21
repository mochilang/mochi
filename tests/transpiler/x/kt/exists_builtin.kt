fun main() {
    val data = mutableListOf(1, 2)
    val flag = run {
    val _res = mutableListOf<Int>()
    for (x in data) {
        if (x == 1) {
            _res.add(x)
        }
    }
    _res
}.isNotEmpty()
    println(flag)
}
