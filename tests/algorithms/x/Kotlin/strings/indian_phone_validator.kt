fun all_digits(s: String): Boolean {
    if (s.length == 0) {
        return false
    }
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s[i].toString()
        if ((c < "0") || (c > "9")) {
            return false
        }
        i = i + 1
    }
    return true
}

fun indian_phone_validator(phone: String): Boolean {
    var s: String = phone
    if ((s.length >= 3) && (s.substring(0, 3) == "+91")) {
        s = s.substring(3, s.length)
        if (s.length > 0) {
            var c: String = s[0].toString()
            if ((c == "-") || (c == " ")) {
                s = s.substring(1, s.length)
            }
        }
    }
    if ((s.length > 0) && (s[0].toString() == "0")) {
        s = s.substring(1, s.length)
    }
    if ((s.length >= 2) && (s.substring(0, 2) == "91")) {
        s = s.substring(2, s.length)
    }
    if (s.length != 10) {
        return false
    }
    var first: String = s[0].toString()
    if (!(((((first == "7") || (first == "8") as Boolean)) || (first == "9")) as Boolean)) {
        return false
    }
    if (!all_digits(s)) {
        return false
    }
    return true
}

fun main() {
    println(indian_phone_validator("+91123456789").toString())
    println(indian_phone_validator("+919876543210").toString())
    println(indian_phone_validator("01234567896").toString())
    println(indian_phone_validator("919876543218").toString())
    println(indian_phone_validator("+91-1234567899").toString())
    println(indian_phone_validator("+91-9876543218").toString())
}
