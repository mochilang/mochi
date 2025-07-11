fun exists(list: Collection<Any?>): Boolean = list.isNotEmpty()
val data = mutableListOf(1, 2)

val flag = exists(run {
    val __res = mutableListOf<Int>()
    for (x in data) {
        if (x == 1) {
            __res.add(x)
        }
    }
    __res
})

fun main() {
    println(flag)
}
