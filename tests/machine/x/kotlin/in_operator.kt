// Code generated from tests/vm/valid/in_operator.mochi

val xs = mutableListOf(1, 2, 3)

fun main() {
    println(2 in xs)
    println(!(5 in xs))
}
