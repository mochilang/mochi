data class Parser(var expr: String, var pos: Int)
data class Res(var v: Int, var p: Parser)
fun skipWS(p: Parser): Parser {
    var i: Int = p.pos
    while ((i < p.expr.length) && (p.expr.substring(i, i + 1) == " ")) {
        i = i + 1
    }
    p.pos = i
    return p
}

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var n: Int = 0
    while (i < str.length) {
        n = ((n * 10) + str.substring(i, i + 1).toInt()) - 48
        i = i + 1
    }
    return n
}

fun parseNumber(p: Parser): Res {
    var p: Parser = p
    p = skipWS(p)
    var start: Int = p.pos
    while (p.pos < p.expr.length) {
        val ch: String = p.expr.substring(p.pos, p.pos + 1)
        if ((ch >= "0") && (ch <= "9")) {
            p.pos = p.pos + 1
        } else {
            break
        }
    }
    val token: String = p.expr.substring(start, p.pos)
    return Res(v = parseIntStr(token), p = p)
}

fun parseFactor(p: Parser): Res {
    var p: Parser = p
    p = skipWS(p)
    if ((p.pos < p.expr.length) && (p.expr.substring(p.pos, p.pos + 1) == "(")) {
        p.pos = p.pos + 1
        var r: Res = parseExpr(p)
        var v = r.v
        p = r.p as Parser
        p = skipWS(p)
        if ((p.pos < p.expr.length) && (p.expr.substring(p.pos, p.pos + 1) == ")")) {
            p.pos = p.pos + 1
        }
        return Res(v = v, p = p)
    }
    if ((p.pos < p.expr.length) && (p.expr.substring(p.pos, p.pos + 1) == "-")) {
        p.pos = p.pos + 1
        var r: Res = parseFactor(p)
        var v = r.v
        p = r.p as Parser
        return Res(v = 0 - (v as Int), p = p)
    }
    return parseNumber(p)
}

fun powInt(base: Int, exp: Int): Int {
    var r: Int = 1
    var b: Int = base
    var e: Int = exp
    while (e > 0) {
        if ((e % 2) == 1) {
            r = r * b
        }
        b = b * b
        e = e / 2.toInt()
    }
    return r
}

fun parsePower(p: Parser): Res {
    var p: Parser = p
    var r: Res = parseFactor(p)
    var v = r.v
    p = r.p as Parser
    while (true) {
        p = skipWS(p)
        if ((p.pos < p.expr.length) && (p.expr.substring(p.pos, p.pos + 1) == "^")) {
            p.pos = p.pos + 1
            var r2: Res = parseFactor(p)
            var rhs = r2.v
            p = r2.p as Parser
            v = powInt(v as Int, rhs as Int)
        } else {
            break
        }
    }
    return Res(v = v, p = p)
}

fun parseTerm(p: Parser): Res {
    var p: Parser = p
    var r: Res = parsePower(p)
    var v = r.v
    p = r.p as Parser
    while (true) {
        p = skipWS(p)
        if (p.pos < p.expr.length) {
            val op: String = p.expr.substring(p.pos, p.pos + 1)
            if (op == "*") {
                p.pos = p.pos + 1
                var r2: Res = parsePower(p)
                var rhs = r2.v
                p = r2.p as Parser
                v = (v as Number).toDouble() * (rhs as Number).toDouble()
                continue
            }
            if (op == "/") {
                p.pos = p.pos + 1
                var r2: Res = parsePower(p)
                var rhs = r2.v
                p = r2.p as Parser
                v = (v as Int) / rhs.toInt()
                continue
            }
        }
        break
    }
    return Res(v = v, p = p)
}

fun parseExpr(p: Parser): Res {
    var p: Parser = p
    var r: Res = parseTerm(p)
    var v = r.v
    p = r.p as Parser
    while (true) {
        p = skipWS(p)
        if (p.pos < p.expr.length) {
            val op: String = p.expr.substring(p.pos, p.pos + 1)
            if (op == "+") {
                p.pos = p.pos + 1
                var r2: Res = parseTerm(p)
                var rhs = r2.v
                p = r2.p as Parser
                v = (v as Number).toDouble() + (rhs as Number).toDouble()
                continue
            }
            if (op == "-") {
                p.pos = p.pos + 1
                var r2: Res = parseTerm(p)
                var rhs = r2.v
                p = r2.p as Parser
                v = (v as Number).toDouble() - (rhs as Number).toDouble()
                continue
            }
        }
        break
    }
    return Res(v = v, p = p)
}

fun evalExpr(expr: String): Int {
    var p: Parser = Parser(expr = expr, pos = 0)
    val r: Res = parseExpr(p as Parser)
    return r.v as Int
}

fun user_main(): Unit {
    val expr: String = "2*(3-1)+2*5"
    println((expr + " = ") + evalExpr(expr).toString())
}

fun main() {
    user_main()
}
