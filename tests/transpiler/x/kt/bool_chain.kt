fun boom(): Boolean {
    println("boom")
    return true
}

fun main() {
    println(if (((1 < 2) && (2 < 3)) && (3 < 4)) 1 else 0)
    println(if (((1 < 2) && (2 > 3)) && boom()) 1 else 0)
    println(if ((((1 < 2) && (2 < 3)) && (3 > 4)) && boom()) 1 else 0)
}
