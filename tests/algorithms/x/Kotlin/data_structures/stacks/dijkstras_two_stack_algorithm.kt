import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

var equation: String = "(5 + ((4 * 2) * (2 + 3)))"
fun is_digit(ch: String): Boolean {
    return ((((((((((((((((((ch == "0") || (ch == "1") as Boolean)) || (ch == "2") as Boolean)) || (ch == "3") as Boolean)) || (ch == "4") as Boolean)) || (ch == "5") as Boolean)) || (ch == "6") as Boolean)) || (ch == "7") as Boolean)) || (ch == "8") as Boolean)) || (ch == "9")) as Boolean
}

fun slice_without_last_int(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun slice_without_last_string(xs: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun dijkstras_two_stack_algorithm(equation: String): Int {
    var operand_stack: MutableList<Int> = mutableListOf<Int>()
    var operator_stack: MutableList<String> = mutableListOf<String>()
    var idx: Int = (0).toInt()
    while (idx < equation.length) {
        var ch: String = equation.substring(idx, idx + 1)
        if ((is_digit(ch)) as Boolean) {
            operand_stack = run { val _tmp = operand_stack.toMutableList(); _tmp.add(ch.toBigInteger().toInt()); _tmp }
        } else {
            if ((((((ch == "+") || (ch == "-") as Boolean)) || (ch == "*") as Boolean)) || (ch == "/")) {
                operator_stack = run { val _tmp = operator_stack.toMutableList(); _tmp.add(ch); _tmp }
            } else {
                if (ch == ")") {
                    var opr: String = operator_stack[operator_stack.size - 1]!!
                    operator_stack = slice_without_last_string(operator_stack)
                    var num1: Int = (operand_stack[operand_stack.size - 1]!!).toInt()
                    operand_stack = slice_without_last_int(operand_stack)
                    var num2: Int = (operand_stack[operand_stack.size - 1]!!).toInt()
                    operand_stack = slice_without_last_int(operand_stack)
                    var total = if (opr == "+") num2 + num1 else if (opr == "-") num2 - num1 else if (opr == "*") num2 * num1 else Math.floorDiv(num2, num1)
                    operand_stack = run { val _tmp = operand_stack.toMutableList(); _tmp.add(total.toInt()); _tmp }
                }
            }
        }
        idx = idx + 1
    }
    return operand_stack[operand_stack.size - 1]!!
}

fun main() {
    println((equation + " = ") + _numToStr(dijkstras_two_stack_algorithm(equation)))
}
