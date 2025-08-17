import java.math.BigInteger

fun panic(msg: String): Nothing {
    throw RuntimeException(msg)
}

fun char_to_value(c: String): Int {
    var digits: String = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var i: Int = (0).toInt()
    while (i < digits.length) {
        if (digits[i].toString() == c) {
            return i
        }
        i = i + 1
    }
    panic("invalid digit")
}

fun int_to_base(number: Int, base: Int): String {
    if ((base < 2) || (base > 36)) {
        panic("'base' must be between 2 and 36 inclusive")
    }
    if (number < 0) {
        panic("number must be a positive integer")
    }
    var digits: String = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var n: Int = (number).toInt()
    var result: String = ""
    while (n > 0) {
        var remainder: Int = (Math.floorMod(n, base)).toInt()
        result = digits[remainder].toString() + result
        n = n / base
    }
    if (result == "") {
        result = "0"
    }
    return result
}

fun base_to_int(num_str: String, base: Int): Int {
    var value: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < num_str.length) {
        var c: String = num_str[i].toString()
        value = (value * base) + char_to_value(c)
        i = i + 1
    }
    return value
}

fun sum_of_digits(num: Int, base: Int): String {
    if ((base < 2) || (base > 36)) {
        panic("'base' must be between 2 and 36 inclusive")
    }
    var num_str: String = int_to_base(num, base)
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < num_str.length) {
        var c: String = num_str[i].toString()
        total = total + char_to_value(c)
        i = i + 1
    }
    return int_to_base(total, base)
}

fun harshad_numbers_in_base(limit: Int, base: Int): MutableList<String> {
    if ((base < 2) || (base > 36)) {
        panic("'base' must be between 2 and 36 inclusive")
    }
    if (limit < 0) {
        return mutableListOf<String>()
    }
    var numbers: MutableList<String> = mutableListOf<String>()
    var i: Int = (1).toInt()
    while (i < limit) {
        var s: String = sum_of_digits(i, base)
        var divisor: Int = (base_to_int(s, base)).toInt()
        if ((Math.floorMod(i, divisor)) == 0) {
            numbers = run { val _tmp = numbers.toMutableList(); _tmp.add(int_to_base(i, base)); _tmp }
        }
        i = i + 1
    }
    return numbers
}

fun is_harshad_number_in_base(num: Int, base: Int): Boolean {
    if ((base < 2) || (base > 36)) {
        panic("'base' must be between 2 and 36 inclusive")
    }
    if (num < 0) {
        return false
    }
    var n: String = int_to_base(num, base)
    var d: String = sum_of_digits(num, base)
    var n_val: Int = (base_to_int(n, base)).toInt()
    var d_val: Int = (base_to_int(d, base)).toInt()
    return (Math.floorMod(n_val, d_val)) == 0
}

fun user_main(): Unit {
    println(int_to_base(0, 21))
    println(int_to_base(23, 2))
    println(int_to_base(58, 5))
    println(int_to_base(167, 16))
    println(sum_of_digits(103, 12))
    println(sum_of_digits(1275, 4))
    println(sum_of_digits(6645, 2))
    println(harshad_numbers_in_base(15, 2))
    println(harshad_numbers_in_base(12, 34))
    println(harshad_numbers_in_base(12, 4))
    println(is_harshad_number_in_base(18, 10))
    println(is_harshad_number_in_base(21, 10))
    println(is_harshad_number_in_base(0 - 21, 5))
}

fun main() {
    user_main()
}
