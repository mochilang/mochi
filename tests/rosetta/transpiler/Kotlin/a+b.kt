fun input(): String = readLine() ?: ""

fun user_main(): Unit {
    val a: Int = (input()).toInt()
    val b: Int = (input()).toInt()
    println(a + b)
}

fun main() {
    user_main()
}
