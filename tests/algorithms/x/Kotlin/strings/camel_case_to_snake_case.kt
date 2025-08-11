fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

var LOWER: String = "abcdefghijklmnopqrstuvwxyz"
var UPPER: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var DIGITS: String = "0123456789"
fun is_lower(ch: String): Boolean {
    var i: Int = (0).toInt()
    while (i < LOWER.length) {
        if (LOWER[i].toString() == ch) {
            return true
        }
        i = i + 1
    }
    return false
}

fun is_upper(ch: String): Boolean {
    var i: Int = (0).toInt()
    while (i < UPPER.length) {
        if (UPPER[i].toString() == ch) {
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

fun is_alpha(ch: String): Boolean {
    if (((is_lower(ch)) as Boolean)) {
        return true
    }
    if (((is_upper(ch)) as Boolean)) {
        return true
    }
    return false
}

fun is_alnum(ch: String): Boolean {
    if (((is_alpha(ch)) as Boolean)) {
        return true
    }
    if (((is_digit(ch)) as Boolean)) {
        return true
    }
    return false
}

fun to_lower(ch: String): String {
    var i: Int = (0).toInt()
    while (i < UPPER.length) {
        if (UPPER[i].toString() == ch) {
            return LOWER[i].toString()
        }
        i = i + 1
    }
    return ch
}

fun camel_to_snake_case(input_str: String): String {
    var snake_str: String = ""
    var i: Int = (0).toInt()
    var prev_is_digit: Boolean = false
    var prev_is_alpha: Boolean = false
    while (i < input_str.length) {
        var ch: String = input_str[i].toString()
        if (((is_upper(ch)) as Boolean)) {
            snake_str = (snake_str + "_") + to_lower(ch)
        } else {
            if (prev_is_digit && is_lower(ch)) {
                snake_str = (snake_str + "_") + ch
            } else {
                if (prev_is_alpha && is_digit(ch)) {
                    snake_str = (snake_str + "_") + ch
                } else {
                    if (!is_alnum(ch)) {
                        snake_str = snake_str + "_"
                    } else {
                        snake_str = snake_str + ch
                    }
                }
            }
        }
        prev_is_digit = is_digit(ch)
        prev_is_alpha = is_alpha(ch)
        i = i + 1
    }
    if ((snake_str.length > 0) && (snake_str[0].toString() == "_")) {
        snake_str = _sliceStr(snake_str, 1, snake_str.length)
    }
    return snake_str
}

fun user_main(): Unit {
    println(camel_to_snake_case("someRandomString"))
    println(camel_to_snake_case("SomeRandomStr#ng"))
    println(camel_to_snake_case("123SomeRandom123String123"))
}

fun main() {
    user_main()
}
