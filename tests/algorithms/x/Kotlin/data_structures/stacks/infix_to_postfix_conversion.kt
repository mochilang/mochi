val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

var PRECEDENCES: MutableMap<String, Int> = mutableMapOf<String, Int>("+" to (1), "-" to (1), "*" to (2), "/" to (2), "^" to (3))
var ASSOCIATIVITIES: MutableMap<String, String> = mutableMapOf<String, String>("+" to ("LR"), "-" to ("LR"), "*" to ("LR"), "/" to ("LR"), "^" to ("RL"))
fun precedence(ch: String): Int {
    if (PRECEDENCES.containsKey(ch)) {
        return (PRECEDENCES)[ch] as Int
    }
    return 0 - 1
}

fun associativity(ch: String): String {
    if (ASSOCIATIVITIES.containsKey(ch)) {
        return (ASSOCIATIVITIES)[ch] as String
    }
    return ""
}

fun balanced_parentheses(expr: String): Boolean {
    var count: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < expr.length) {
        var ch: String = expr.substring(i, i + 1)
        if (ch == "(") {
            count = count + 1
        }
        if (ch == ")") {
            count = count - 1
            if (count < 0) {
                return false
            }
        }
        i = i + 1
    }
    return count == 0
}

fun is_letter(ch: String): Boolean {
    return (((("a" <= ch) && (ch <= "z") as Boolean)) || ((("A" <= ch) && (ch <= "Z") as Boolean))) as Boolean
}

fun is_digit(ch: String): Boolean {
    return (("0" <= ch) && (ch <= "9")) as Boolean
}

fun is_alnum(ch: String): Boolean {
    return (is_letter(ch) || is_digit(ch)) as Boolean
}

fun infix_to_postfix(expression: String): String {
    if (balanced_parentheses(expression) == false) {
        panic("Mismatched parentheses")
    }
    var stack: MutableList<String> = mutableListOf<String>()
    var postfix: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < expression.length) {
        var ch: String = expression.substring(i, i + 1)
        if ((is_alnum(ch)) as Boolean) {
            postfix = run { val _tmp = postfix.toMutableList(); _tmp.add(ch); _tmp }
        } else {
            if (ch == "(") {
                stack = run { val _tmp = stack.toMutableList(); _tmp.add(ch); _tmp }
            } else {
                if (ch == ")") {
                    while ((stack.size > 0) && (stack[stack.size - 1]!! != "(")) {
                        postfix = run { val _tmp = postfix.toMutableList(); _tmp.add(stack[stack.size - 1]!!); _tmp }
                        stack = _sliceList(stack, 0, stack.size - 1)
                    }
                    if (stack.size > 0) {
                        stack = _sliceList(stack, 0, stack.size - 1)
                    }
                } else {
                    if (ch == " ") {
                    } else {
                        while (true) {
                            if (stack.size == 0) {
                                stack = run { val _tmp = stack.toMutableList(); _tmp.add(ch); _tmp }
                                break
                            }
                            var cp: Int = (precedence(ch)).toInt()
                            var tp: Int = (precedence(stack[stack.size - 1]!!)).toInt()
                            if (cp > tp) {
                                stack = run { val _tmp = stack.toMutableList(); _tmp.add(ch); _tmp }
                                break
                            }
                            if (cp < tp) {
                                postfix = run { val _tmp = postfix.toMutableList(); _tmp.add(stack[stack.size - 1]!!); _tmp }
                                stack = _sliceList(stack, 0, stack.size - 1)
                                continue
                            }
                            if (associativity(ch) == "RL") {
                                stack = run { val _tmp = stack.toMutableList(); _tmp.add(ch); _tmp }
                                break
                            }
                            postfix = run { val _tmp = postfix.toMutableList(); _tmp.add(stack[stack.size - 1]!!); _tmp }
                            stack = _sliceList(stack, 0, stack.size - 1)
                        }
                    }
                }
            }
        }
        i = i + 1
    }
    while (stack.size > 0) {
        postfix = run { val _tmp = postfix.toMutableList(); _tmp.add(stack[stack.size - 1]!!); _tmp }
        stack = _sliceList(stack, 0, stack.size - 1)
    }
    var res: String = ""
    var j: Int = (0).toInt()
    while (j < postfix.size) {
        if (j > 0) {
            res = res + " "
        }
        res = res + postfix[j]!!
        j = j + 1
    }
    return res
}

fun user_main(): Unit {
    var expression: String = "a+b*(c^d-e)^(f+g*h)-i"
    println(expression)
    println(infix_to_postfix(expression))
}

fun main() {
    user_main()
}
