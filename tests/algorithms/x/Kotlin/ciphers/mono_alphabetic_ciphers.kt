var LETTERS: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
fun find_char(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun encrypt_message(key: String, message: String): String {
    var chars_a: String = key
    var chars_b: String = LETTERS
    var translated: String = ""
    var i: Int = 0
    while (i < message.length) {
        var symbol: String = message[i].toString()
        var upper_sym: String = (symbol.toUpperCase() as String)
        var sym_index: Int = find_char(chars_a, upper_sym)
        if (sym_index >= 0) {
            var sub_char: String = chars_b[sym_index].toString()
            if (symbol == upper_sym) {
                translated = translated + (sub_char.toUpperCase()).toString()
            } else {
                translated = translated + (sub_char.toLowerCase()).toString()
            }
        } else {
            translated = translated + symbol
        }
        i = i + 1
    }
    return translated
}

fun decrypt_message(key: String, message: String): String {
    var chars_a: String = LETTERS
    var chars_b: String = key
    var translated: String = ""
    var i: Int = 0
    while (i < message.length) {
        var symbol: String = message[i].toString()
        var upper_sym: String = (symbol.toUpperCase() as String)
        var sym_index: Int = find_char(chars_a, upper_sym)
        if (sym_index >= 0) {
            var sub_char: String = chars_b[sym_index].toString()
            if (symbol == upper_sym) {
                translated = translated + (sub_char.toUpperCase()).toString()
            } else {
                translated = translated + (sub_char.toLowerCase()).toString()
            }
        } else {
            translated = translated + symbol
        }
        i = i + 1
    }
    return translated
}

fun user_main(): Unit {
    var message: String = "Hello World"
    var key: String = "QWERTYUIOPASDFGHJKLZXCVBNM"
    var mode: String = "decrypt"
    var translated: String = ""
    if (mode == "encrypt") {
        translated = encrypt_message(key, message)
    } else {
        if (mode == "decrypt") {
            translated = decrypt_message(key, message)
        }
    }
    println((((("Using the key " + key) + ", the ") + mode) + "ed message is: ") + translated)
}

fun main() {
    user_main()
}
