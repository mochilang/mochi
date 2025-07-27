import java.math.BigInteger

val target: Int = 269696
val modulus: Int = 1000000
var n: Int = 1
fun main() {
    while (true) {
        val square: Int = n * n
        val ending: Int = Math.floorMod(square, modulus)
        if (ending == target) {
            println((("The smallest number whose square ends with " + target.toString()) + " is ") + n.toString())
            break
        }
        n = n + 1
    }
}
