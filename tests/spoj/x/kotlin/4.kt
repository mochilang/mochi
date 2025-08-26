val _dataDir = "/workspace/mochi/tests/spoj/x/mochi"

fun input(): String = readLine() ?: ""

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

fun precedence(op: String): Int {
    if ((op == "+") || (op == "-")) {
        return 1
    }
    if ((op == "*") || (op == "/")) {
        return 2
    }
    if (op == "^") {
        return 3
    }
    return 0
}

fun popTop(stack: MutableList<String>): String {
    return stack[stack.size - 1]!!
}

fun popStack(stack: MutableList<String>): MutableList<String> {
    var newStack: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < (stack.size - 1)) {
        newStack = run { val _tmp = newStack.toMutableList(); _tmp.add(stack[i]!!); _tmp }
        i = i + 1
    }
    return newStack
}

fun toRPN(expr: String): String {
    var out: String = ""
    var stack: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < expr.length) {
        var ch: String = expr[i].toString()
        if ((ch >= "a") && (ch <= "z")) {
            out = out + ch
        } else {
            if (ch == "(") {
                stack = run { val _tmp = stack.toMutableList(); _tmp.add(ch); _tmp }
            } else {
                if (ch == ")") {
                    while (stack.size > 0) {
                        var top: String = popTop(stack)
                        if (top == "(") {
                            stack = popStack(stack)
                            break
                        }
                        out = out + top
                        stack = popStack(stack)
                    }
                } else {
                    var prec: Int = (precedence(ch)).toInt()
                    while (stack.size > 0) {
                        var top: String = popTop(stack)
                        if (top == "(") {
                            break
                        }
                        var topPrec: Int = (precedence(top)).toInt()
                        if ((topPrec > prec) || (((topPrec == prec) && (ch != "^") as Boolean))) {
                            out = out + top
                            stack = popStack(stack)
                        } else {
                            break
                        }
                    }
                    stack = run { val _tmp = stack.toMutableList(); _tmp.add(ch); _tmp }
                }
            }
        }
        i = i + 1
    }
    while (stack.size > 0) {
        var top: String = popTop(stack)
        out = out + top
        stack = popStack(stack)
    }
    return out
}

fun user_main(): Unit {
    var t: Int = ((input()).toBigInteger().toInt()).toInt()
    var i: Int = (0).toInt()
    while (i < t) {
        var expr: String = input()
        println(toRPN(expr))
        i = i + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
