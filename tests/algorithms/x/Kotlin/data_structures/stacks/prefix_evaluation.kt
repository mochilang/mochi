import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

var test_expression: String = "+ 9 * 2 6"
fun split_custom(s: String, sep: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if (ch == sep) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
            current = ""
        } else {
            current = current + ch
        }
        i = i + 1
    }
    res = run { val _tmp = res.toMutableList(); _tmp.add(current); _tmp }
    return res
}

fun tokenize(s: String): MutableList<String> {
    var parts: MutableList<String> = split_custom(s, " ")
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < parts.size) {
        var p: String = parts[i]!!
        if (p != "") {
            res = run { val _tmp = res.toMutableList(); _tmp.add(p); _tmp }
        }
        i = i + 1
    }
    return res
}

fun is_digit(ch: String): Boolean {
    return ((ch >= "0") && (ch <= "9")) as Boolean
}

fun is_operand(token: String): Boolean {
    if (token == "") {
        return false
    }
    var i: Int = (0).toInt()
    while (i < token.length) {
        var ch: String = token.substring(i, i + 1)
        if (!is_digit(ch)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun to_int(token: String): Int {
    var res: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < token.length) {
        res = (res * 10) + (token.substring(i, i + 1).toBigInteger().toInt())
        i = i + 1
    }
    return res
}

fun apply_op(op: String, a: Double, b: Double): Double {
    if (op == "+") {
        return a + b
    }
    if (op == "-") {
        return a - b
    }
    if (op == "*") {
        return a * b
    }
    if (op == "/") {
        return a / b
    }
    return 0.0
}

fun evaluate(expression: String): Double {
    var tokens: MutableList<String> = tokenize(expression)
    var stack: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (tokens.size - 1).toInt()
    while (i >= 0) {
        var token: String = tokens[i]!!
        if (token != "") {
            if ((is_operand(token)) as Boolean) {
                stack = run { val _tmp = stack.toMutableList(); _tmp.add((to_int(token)).toDouble()); _tmp }
            } else {
                var o1: Double = stack[stack.size - 1]!!
                var o2: Double = stack[stack.size - 2]!!
                stack = _sliceList(stack, 0, stack.size - 2)
                var res: Double = apply_op(token, o1, o2)
                stack = run { val _tmp = stack.toMutableList(); _tmp.add(res); _tmp }
            }
        }
        i = i - 1
    }
    return stack[0]!!
}

fun eval_rec(tokens: MutableList<String>, pos: Int): MutableList<Double> {
    var token: String = tokens[pos]!!
    var next: Int = (pos + 1).toInt()
    if ((is_operand(token)) as Boolean) {
        return mutableListOf((to_int(token)).toDouble(), next.toDouble())
    }
    var left: MutableList<Double> = eval_rec(tokens, next)
    var a: Double = left[0]!!
    var p1: Int = ((left[1]!!).toInt()).toInt()
    var right: MutableList<Double> = eval_rec(tokens, p1)
    var b: Double = right[0]!!
    var p2: Double = right[1]!!
    return mutableListOf(apply_op(token, a, b), p2)
}

fun evaluate_recursive(expression: String): Double {
    var tokens: MutableList<String> = tokenize(expression)
    var res: MutableList<Double> = eval_rec(tokens, 0)
    return res[0]!!
}

fun main() {
    println(_numToStr(evaluate(test_expression)))
    var test_expression2: String = "/ * 10 2 + 4 1 "
    println(_numToStr(evaluate(test_expression2)))
    var test_expression3: String = "+ * 2 3 / 8 4"
    println(_numToStr(evaluate_recursive(test_expression3)))
}
