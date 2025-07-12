// Code generated from tests/vm/valid/binary_precedence.mochi

fun main() {
    println(1 + 2 * 3)
    println((1 + 2) * 3)
    println(2 * 3 + 1)
    println(2 * (3 + 1))
}
