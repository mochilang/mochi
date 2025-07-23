var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        _nowSeed.toInt()
    } else {
        System.nanoTime().toInt()
    }
}

fun input(): String = readLine() ?: ""

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    val digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + ((digits)[str.substring(i, i + 1)]!! as Int) as Int
        i = i + 1
    }
    if (neg as Boolean) {
        n = 0 - n
    }
    return n
}

fun user_main(): Unit {
    var total: Int = 0
    var computer: Boolean = (_now() % 2) == 0
    println("Enter q to quit at any time\n")
    if (computer as Boolean) {
        println("The computer will choose first")
    } else {
        println("You will choose first")
    }
    println("\n\nRunning total is now 0\n\n")
    var round: Int = 1
    var done: Boolean = false
    while (!done) {
        println(("ROUND " + round.toString()) + ":\n\n")
        var i: Int = 0
        while ((i < 2) && (!done as Boolean)) {
            if (computer as Boolean) {
                var choice: Int = 0
                if (total < 18) {
                    choice = (_now() % 3) + 1
                } else {
                    choice = 21 - total
                }
                total = total + choice
                println("The computer chooses " + choice.toString())
                println("Running total is now " + total.toString())
                if (total == 21) {
                    println("\nSo, commiserations, the computer has won!")
                    done = true
                }
            } else {
                while (true) {
                    println("Your choice 1 to 3 : ")
                    val line: String = input()
                    if ((line == "q") || (line == "Q")) {
                        println("OK, quitting the game")
                        done = true
                        break
                    }
                    var num: Int = parseIntStr(line)
                    if ((num < 1) || (num > 3)) {
                        if ((total + num) > 21) {
                            println("Too big, try again")
                        } else {
                            println("Out of range, try again")
                        }
                        continue
                    }
                    if ((total + num) > 21) {
                        println("Too big, try again")
                        continue
                    }
                    total = total + num
                    println("Running total is now " + total.toString())
                    break
                }
                if (total == 21) {
                    println("\nSo, congratulations, you've won!")
                    done = true
                }
            }
            println("\n")
            computer = !computer as Boolean
            i = i + 1
        }
        round = round + 1
    }
}

fun main() {
    user_main()
}
