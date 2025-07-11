val data = mutableListOf(1, 2)

val flag = run {
    val __res = mutableListOf<Int>()
    for (x in data) {
        if (x == 1) {
            __res.add(x)
        }
    }
    __res
}.isNotEmpty()

fun main() {
    println(flag)
}
