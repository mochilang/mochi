fun lshift(x: Int, n: Int): Int {
return x * pow2(n)
}

fun pow2(n: Int): Int {
var v = 1
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

fun rshift(x: Int, n: Int): Int {
return x / pow2(n)
}

fun ord(ch: String): Int {
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var digits: String = "0123456789"
    var i: Int = (0).toInt()
    while (i < lower.length) {
        if (lower[i].toString() == ch) {
            return 97 + i
        }
        i = i + 1
    }
    i = 0
    while (i < upper.length) {
        if (upper[i].toString() == ch) {
            return 65 + i
        }
        i = i + 1
    }
    i = 0
    while (i < digits.length) {
        if (digits[i].toString() == ch) {
            return 48 + i
        }
        i = i + 1
    }
    if (ch == " ") {
        return 32
    }
    if (ch == "_") {
        return 95
    }
    if (ch == ".") {
        return 46
    }
    if (ch == "'") {
        return 39
    }
    return 0
}

fun is_contains_unique_chars(input_str: String): Boolean {
    var bitmap: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < input_str.length) {
        var code: Int = (ord(input_str[i].toString())).toInt()
        if ((Math.floorMod(rshift(bitmap, code), 2)) == 1) {
            return false
        }
        bitmap = bitmap + lshift(1, code)
        i = i + 1
    }
    return true
}

fun main() {
    println(is_contains_unique_chars("I_love.py").toString())
    println(is_contains_unique_chars("I don't love Python").toString())
}
