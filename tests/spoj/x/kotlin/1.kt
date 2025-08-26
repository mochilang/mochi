val _dataDir = "/workspace/mochi/tests/spoj/x/mochi"

fun input(): String = readLine() ?: ""

fun user_main(): Unit {
    while (true) {
        var line: String = input()
        var n: Int = (line.toBigInteger().toInt()).toInt()
        if (n == 42) {
            break
        }
        println(line)
    }
}

fun main() {
    user_main()
}
