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

fun randDigit(): Int {
    return (_now() % 9) + 1
}

fun user_main(): Unit {
    var digits: MutableList<Any?> = mutableListOf()
    for (i in 0 until 4) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(randDigit()); _tmp } as MutableList<Any?>
    }
    var numstr: String = ""
    for (i in 0 until 4) {
        numstr = numstr + (digits[i]).toString()
    }
    println(("Your numbers: " + numstr) + "\n")
    println("Enter RPN: ")
    var expr: String = input()
    if (expr.length != 7) {
        println("invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)")
        return
    }
    var stack: MutableList<Any?> = mutableListOf()
    var i: Int = 0
    var valid: Boolean = true
    while (i < expr.length) {
        val ch: String = expr.substring(i, i + 1)
        if ((ch >= "0") && (ch <= "9")) {
            if (digits.size == 0) {
                println("too many numbers.")
                return
            }
            var j: Int = 0
            while (digits[j] != (ch.toInt() - "0".toInt())) {
                j = j + 1
                if (j == digits.size) {
                    println("wrong numbers.")
                    return
                }
            }
            digits = (digits.subList(0, j) + digits.subList(j + 1, digits.size)).toMutableList()
            stack = run { val _tmp = stack.toMutableList(); _tmp.add((ch.toInt() - "0".toInt()).toDouble()); _tmp } as MutableList<Any?>
        } else {
            if (stack.size < 2) {
                println("invalid expression syntax.")
                valid = false
                break
            }
            var b: Any? = stack[stack.size - 1]
            var a: Any? = stack[stack.size - 2]
            if (ch == "+") {
                stack[stack.size - 2] = (a as Number).toDouble() + (b as Number).toDouble()
            } else {
                if (ch == "-") {
                    stack[stack.size - 2] = (a as Number).toDouble() - (b as Number).toDouble()
                } else {
                    if (ch == "*") {
                        stack[stack.size - 2] = (a as Number).toDouble() * (b as Number).toDouble()
                    } else {
                        if (ch == "/") {
                            stack[stack.size - 2] = (a as Number).toDouble() / (b as Number).toDouble()
                        } else {
                            println(ch + " invalid.")
                            valid = false
                            break
                        }
                    }
                }
            }
            stack = stack.subList(0, stack.size - 1)
        }
        i = i + 1
    }
    if (valid as Boolean) {
        if (kotlin.math.abs((stack[0] as Number).toDouble() - 24.0) > 0.000001) {
            println(("incorrect. " + (stack[0]).toString()) + " != 24")
        } else {
            println("correct.")
        }
    }
}

fun main() {
    user_main()
}
