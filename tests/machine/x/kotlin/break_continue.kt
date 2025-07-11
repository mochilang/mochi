val numbers = mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9)

fun main() {
    for (n in numbers) {
        if (n % 2 == 0) {
            continue
        }
        if (n > 7) {
            break
        }
        println(listOf("odd number:", n).joinToString(" "))
    }
}
