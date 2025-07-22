fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

fun test_addition_works(): Unit {
    val x = 1 + 2
    expect(x == 3)
}

fun main() {
    test_addition_works()
    println("ok")
}
