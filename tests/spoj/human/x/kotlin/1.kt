// Solution for SPOJ TEST - Life, the Universe, and Everything
// https://www.spoj.com/problems/TEST
import java.util.Scanner

fun main() {
    val scanner = Scanner(System.`in`)
    val sb = StringBuilder()
    while (scanner.hasNextInt()) {
        val n = scanner.nextInt()
        if (n == 42) break
        sb.appendLine(n)
    }
    print(sb.toString())
}
