fun zeroval(ival: Int): Int {
    var x: Int = ival
    x = 0
    return x
}

fun zeroptr(ref: MutableList<Int>): Unit {
    ref[0] = 0
}

fun user_main(): Unit {
    var i: Int = 1
    println("initial: " + i.toString())
    var tmp: Int = zeroval(i)
    println("zeroval: " + i.toString())
    var box: MutableList<Int> = mutableListOf(i)
    zeroptr(box)
    i = box[0]!!
    println("zeroptr: " + i.toString())
    println("pointer: 0")
}

fun main() {
    user_main()
}
