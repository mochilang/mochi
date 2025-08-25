val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

var tests: MutableList<String> = mutableListOf("([]{})", "[()]{}{[()()]()}", "[(])", "1+2*3-4", "")
var idx: Int = (0).toInt()
fun pop_last(xs: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun balanced_parentheses(s: String): Boolean {
    var stack: MutableList<String> = mutableListOf<String>()
    var pairs: MutableMap<String, String> = mutableMapOf<String, String>("(" to (")"), "[" to ("]"), "{" to ("}")) as MutableMap<String, String>
    var i: Int = (0).toInt()
    while (i < s.length) {
        var ch: String = s[i].toString()
        if (pairs.containsKey(ch)) {
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(ch); _tmp }
        } else {
            if ((((ch == ")") || (ch == "]") as Boolean)) || (ch == "}")) {
                if (stack.size == 0) {
                    return false
                }
                var top: String = stack[stack.size - 1]!!
                if ((pairs)[top] as String != ch) {
                    return false
                }
                stack = pop_last(stack)
            }
        }
        i = i + 1
    }
    return stack.size == 0
}

fun main() {
    while (idx < tests.size) {
        println(balanced_parentheses(tests[idx]!!))
        idx = (idx + 1).toInt()
    }
}
