var square: MutableList<MutableList<String>> = mutableListOf(mutableListOf("a", "b", "c", "d", "e"), mutableListOf("f", "g", "h", "i", "k"), mutableListOf("l", "m", "n", "o", "p"), mutableListOf("q", "r", "s", "t", "u"), mutableListOf("v", "w", "x", "y", "z"))
fun letter_to_numbers(letter: String): MutableList<Int> {
    var i: Int = 0
    while (i < square.size) {
        var j: Int = 0
        while (j < (square[i]!!).size) {
            if ((((square[i]!!) as MutableList<String>))[j]!! == letter) {
                return mutableListOf(i + 1, j + 1)
            }
            j = j + 1
        }
        i = i + 1
    }
    return mutableListOf(0, 0)
}

fun numbers_to_letter(index1: Int, index2: Int): String {
    return (((square[index1 - 1]!!) as MutableList<String>))[index2 - 1]!!
}

fun char_to_int(ch: String): Int {
    if (ch == "1") {
        return 1
    }
    if (ch == "2") {
        return 2
    }
    if (ch == "3") {
        return 3
    }
    if (ch == "4") {
        return 4
    }
    if (ch == "5") {
        return 5
    }
    return 0
}

fun encode(message: String): String {
    var message: String = message
    message = (message.toLowerCase() as String)
    var encoded: String = ""
    var i: Int = 0
    while (i < message.length) {
        var ch: String = message[i].toString()
        if (ch == "j") {
            ch = "i"
        }
        if (ch != " ") {
            var nums: MutableList<Int> = letter_to_numbers(ch)
            encoded = (encoded + (nums[0]!!).toString()) + (nums[1]!!).toString()
        } else {
            encoded = encoded + " "
        }
        i = i + 1
    }
    return encoded
}

fun decode(message: String): String {
    var decoded: String = ""
    var i: Int = 0
    while (i < message.length) {
        if (message[i].toString() == " ") {
            decoded = decoded + " "
            i = i + 1
        } else {
            var index1: Int = char_to_int(message[i].toString())
            var index2: Int = char_to_int(message[i + 1].toString())
            var letter: String = numbers_to_letter(index1, index2)
            decoded = decoded + letter
            i = i + 2
        }
    }
    return decoded
}

fun main() {
    println(encode("test message"))
    println(encode("Test Message"))
    println(decode("44154344 32154343112215"))
    println(decode("4415434432154343112215"))
}
