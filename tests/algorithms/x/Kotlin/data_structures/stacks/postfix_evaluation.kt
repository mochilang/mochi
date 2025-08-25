val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun slice_without_last(xs: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun parse_float(token: String): Double {
    var sign: Double = 1.0
    var idx: Int = (0).toInt()
    if (token.length > 0) {
        var first: String = token.substring(0, 1)
        if (first == "-") {
            sign = 0.0 - 1.0
            idx = 1
        } else {
            if (first == "+") {
                idx = 1
            }
        }
    }
    var int_part: Int = (0).toInt()
    while ((idx < token.length) && (token.substring(idx, idx + 1) != ".")) {
        int_part = (int_part * 10) + (token.substring(idx, idx + 1).toBigInteger().toInt())
        idx = idx + 1
    }
    var result: Double = 1.0 * (int_part).toDouble()
    if ((idx < token.length) && (token.substring(idx, idx + 1) == ".")) {
        idx = idx + 1
        var place: Double = 0.1
        while (idx < token.length) {
            var digit: Int = (token.substring(idx, idx + 1).toBigInteger().toInt()).toInt()
            result = result + (place * (1.0 * (digit).toDouble()))
            place = place / 10.0
            idx = idx + 1
        }
    }
    return sign * result
}

fun pow_float(base: Double, exp: Double): Double {
    var result: Double = 1.0
    var i: Int = (0).toInt()
    var e: Int = (exp.toInt()).toInt()
    while (i < e) {
        result = result * base
        i = i + 1
    }
    return result
}

fun apply_op(a: Double, b: Double, op: String): Double {
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
    if (op == "^") {
        return pow_float(a, b)
    }
    return 0.0
}

fun evaluate(tokens: MutableList<String>): Double {
    if (tokens.size == 0) {
        return 0.0
    }
    var stack: MutableList<Double> = mutableListOf<Double>()
    for (token in tokens) {
        if ((((((((token == "+") || (token == "-") as Boolean)) || (token == "*") as Boolean)) || (token == "/") as Boolean)) || (token == "^")) {
            if ((((token == "+") || (token == "-") as Boolean)) && (stack.size < 2)) {
                var b: Double = stack[stack.size - 1]!!
                stack = slice_without_last(stack)
                if (token == "-") {
                    stack = run { val _tmp = stack.toMutableList(); _tmp.add(0.0 - b); _tmp }
                } else {
                    stack = run { val _tmp = stack.toMutableList(); _tmp.add(b); _tmp }
                }
            } else {
                var b: Double = stack[stack.size - 1]!!
                stack = slice_without_last(stack)
                var a: Double = stack[stack.size - 1]!!
                stack = slice_without_last(stack)
                var result: Double = apply_op(a, b, token)
                stack = run { val _tmp = stack.toMutableList(); _tmp.add(result); _tmp }
            }
        } else {
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(parse_float(token)); _tmp }
        }
    }
    if (stack.size != 1) {
        panic("Invalid postfix expression")
    }
    return stack[0]!!
}

fun main() {
    println(_numToStr(evaluate(mutableListOf("2", "1", "+", "3", "*"))))
    println(_numToStr(evaluate(mutableListOf("4", "13", "5", "/", "+"))))
    println(_numToStr(evaluate(mutableListOf("5", "6", "9", "*", "+"))))
    println(_numToStr(evaluate(mutableListOf("2", "-", "3", "+"))))
    println(_numToStr(evaluate(mutableListOf<String>())))
}
