import java.math.BigInteger

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

fun validate_initial_digits(cc: String): Boolean {
    return (((((((((((_sliceStr(cc, 0, 2) == "34") || (_sliceStr(cc, 0, 2) == "35") as Boolean)) || (_sliceStr(cc, 0, 2) == "37") as Boolean)) || (_sliceStr(cc, 0, 1) == "4") as Boolean)) || (_sliceStr(cc, 0, 1) == "5") as Boolean)) || (_sliceStr(cc, 0, 1) == "6")) as Boolean)
}

fun luhn_validation(cc: String): Boolean {
    var sum: Int = (0).toInt()
    var double_digit: Boolean = false
    var i: BigInteger = ((cc.length - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        var n: Int = ((_sliceStr(cc, (i).toInt(), (i.add((1).toBigInteger())).toInt()).toBigInteger().toInt())).toInt()
        if ((double_digit as Boolean)) {
            n = n * 2
            if (n > 9) {
                n = n - 9
            }
        }
        sum = sum + n
        double_digit = (!double_digit as Boolean)
        i = i.subtract((1).toBigInteger())
    }
    return (Math.floorMod(sum, 10)) == 0
}

fun is_digit_string(s: String): Boolean {
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = _sliceStr(s, i, i + 1)
        if ((c < "0") || (c > "9")) {
            return false
        }
        i = i + 1
    }
    return true
}

fun validate_credit_card_number(cc: String): Boolean {
    var error_message: String = cc + " is an invalid credit card number because"
    if (!is_digit_string(cc)) {
        println(error_message + " it has nonnumerical characters.")
        return false
    }
    if (!(((cc.length >= 13) && (cc.length <= 16)) as Boolean)) {
        println(error_message + " of its length.")
        return false
    }
    if (!validate_initial_digits(cc)) {
        println(error_message + " of its first two digits.")
        return false
    }
    if (!luhn_validation(cc)) {
        println(error_message + " it fails the Luhn check.")
        return false
    }
    println(cc + " is a valid credit card number.")
    return true
}

fun user_main(): Unit {
    validate_credit_card_number("4111111111111111")
    validate_credit_card_number("32323")
}

fun main() {
    user_main()
}
