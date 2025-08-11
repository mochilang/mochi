fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun has_alpha(s: String): Boolean {
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        if ((((c >= "a") && (c <= "z") as Boolean)) || (((c >= "A") && (c <= "Z") as Boolean))) {
            return true
        }
        i = i + 1
    }
    return false
}

fun parse_decimal(s: String): Int {
    var value: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        if ((c < "0") || (c > "9")) {
            panic("Non-digit character encountered")
        }
        value = (value * 10) + ((c.toBigInteger().toInt()))
        i = i + 1
    }
    return value
}

fun get_barcode(barcode: String): Int {
    if (((has_alpha(barcode)) as Boolean)) {
        panic(("Barcode '" + barcode) + "' has alphabetic characters.")
    }
    if ((barcode.length > 0) && (barcode[0].toString() == "-")) {
        panic("The entered barcode has a negative value. Try again.")
    }
    return parse_decimal(barcode)
}

fun get_check_digit(barcode: Int): Int {
    var num: Int = (barcode / 10).toInt()
    var s: Int = (0).toInt()
    var position: Int = (0).toInt()
    while (num != 0) {
        var mult: Int = (if ((Math.floorMod(position, 2)) == 0) 3 else 1).toInt()
        s = s + (mult * (Math.floorMod(num, 10)))
        num = num / 10
        position = position + 1
    }
    return Math.floorMod((10 - (Math.floorMod(s, 10))), 10)
}

fun is_valid(barcode: Int): Boolean {
    return (((barcode.toString().length == 13) && (get_check_digit(barcode) == (Math.floorMod(barcode, 10)))) as Boolean)
}

fun main() {
    println(get_check_digit((8718452538119L.toInt())).toString())
    println(get_check_digit(87184523).toString())
    println(get_check_digit((87193425381086L.toInt())).toString())
    var res: MutableList<Int> = mutableListOf<Int>()
    var x: Int = (0).toInt()
    while (x < 100) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(get_check_digit(x)); _tmp }
        x = (x + 10).toInt()
    }
    println(res)
    println(is_valid((8718452538119L.toInt())).toString())
    println(is_valid(87184525).toString())
    println(is_valid((87193425381089L.toInt())).toString())
    println(is_valid(0).toString())
    println(get_barcode("8718452538119").toString())
}
