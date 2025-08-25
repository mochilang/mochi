import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

var PRIORITY: MutableMap<String, Int> = mutableMapOf<String, Int>("^" to (3), "*" to (2), "/" to (2), "%" to (2), "+" to (1), "-" to (1))
var LETTERS: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
var DIGITS: String = "0123456789"
fun is_alpha(ch: String): Boolean {
    var i: Int = (0).toInt()
    while (i < LETTERS.length) {
        if (LETTERS[i].toString() == ch) {
            return true
        }
        i = i + 1
    }
    return false
}

fun is_digit(ch: String): Boolean {
    var i: Int = (0).toInt()
    while (i < DIGITS.length) {
        if (DIGITS[i].toString() == ch) {
            return true
        }
        i = i + 1
    }
    return false
}

fun reverse_string(s: String): String {
    var out: String = ""
    var i: Int = (s.length - 1).toInt()
    while (i >= 0) {
        out = out + s[i].toString()
        i = i - 1
    }
    return out
}

fun infix_to_postfix(infix: String): String {
    var stack: MutableList<String> = mutableListOf<String>()
    var post: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < infix.length) {
        var x: String = infix[i].toString()
        if (is_alpha(x) || is_digit(x)) {
            post = run { val _tmp = post.toMutableList(); _tmp.add(x); _tmp }
        } else {
            if (x == "(") {
                stack = run { val _tmp = stack.toMutableList(); _tmp.add(x); _tmp }
            } else {
                if (x == ")") {
                    if (stack.size == 0) {
                        panic("list index out of range")
                    }
                    while (stack[stack.size - 1]!! != "(") {
                        post = run { val _tmp = post.toMutableList(); _tmp.add(stack[stack.size - 1]!!); _tmp }
                        stack = _sliceList(stack, 0, stack.size - 1)
                    }
                    stack = _sliceList(stack, 0, stack.size - 1)
                } else {
                    if (stack.size == 0) {
                        stack = run { val _tmp = stack.toMutableList(); _tmp.add(x); _tmp }
                    } else {
                        while ((((stack.size > 0) && (stack[stack.size - 1]!! != "(") as Boolean)) && ((PRIORITY)[x] as Int <= (PRIORITY)[stack[stack.size - 1]!!] as Int)) {
                            post = run { val _tmp = post.toMutableList(); _tmp.add(stack[stack.size - 1]!!); _tmp }
                            stack = _sliceList(stack, 0, stack.size - 1)
                        }
                        stack = run { val _tmp = stack.toMutableList(); _tmp.add(x); _tmp }
                    }
                }
            }
        }
        i = i + 1
    }
    while (stack.size > 0) {
        if (stack[stack.size - 1]!! == "(") {
            panic("invalid expression")
        }
        post = run { val _tmp = post.toMutableList(); _tmp.add(stack[stack.size - 1]!!); _tmp }
        stack = _sliceList(stack, 0, stack.size - 1)
    }
    var res: String = ""
    var j: Int = (0).toInt()
    while (j < post.size) {
        res = res + post[j]!!
        j = j + 1
    }
    return res
}

fun infix_to_prefix(infix: String): String {
    var reversed: String = ""
    var i: Int = (infix.length - 1).toInt()
    while (i >= 0) {
        var ch: String = infix[i].toString()
        if (ch == "(") {
            reversed = reversed + ")"
        } else {
            if (ch == ")") {
                reversed = reversed + "("
            } else {
                reversed = reversed + ch
            }
        }
        i = i - 1
    }
    var postfix: String = infix_to_postfix(reversed)
    var prefix: String = reverse_string(postfix)
    return prefix
}

fun test_simple_expression(): Unit {
    expect(infix_to_prefix("a+b^c") == "+a^bc")
}

fun test_complex_expression(): Unit {
    expect(infix_to_prefix("1*((-a)*2+b)") == "*1+*-a2b")
}

fun main() {
    test_simple_expression()
    test_complex_expression()
}
