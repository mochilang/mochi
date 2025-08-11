import java.math.BigInteger

fun strip_and_remove_spaces(s: String): String {
    var start: Int = (0).toInt()
    var end: BigInteger = ((s.length - 1).toBigInteger())
    while ((start < s.length) && (s[start].toString() == " ")) {
        start = start + 1
    }
    while ((end.compareTo((start).toBigInteger()) >= 0) && (s[(end).toInt()].toString() == " ")) {
        end = end.subtract((1).toBigInteger())
    }
    var res: String = ""
    var i: Int = (start).toInt()
    while ((i).toBigInteger().compareTo((end)) <= 0) {
        var ch: String = s[i].toString()
        if (ch != " ") {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun check_anagrams(a: String, b: String): Boolean {
    var s1: String = (a.toLowerCase() as String)
    var s2: String = (b.toLowerCase() as String)
    s1 = strip_and_remove_spaces(s1)
    s2 = strip_and_remove_spaces(s2)
    if (s1.length != s2.length) {
        return false
    }
    var count: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var i: Int = (0).toInt()
    while (i < s1.length) {
        var c1: String = s1[i].toString()
        var c2: String = s2[i].toString()
        if (c1 in count) {
            (count)[c1] = (count)[c1] as Int + 1
        } else {
            (count)[c1] = 1
        }
        if (c2 in count) {
            (count)[c2] = (count)[c2] as Int - 1
        } else {
            (count)[c2] = 0 - 1
        }
        i = i + 1
    }
    for (ch in count.keys) {
        if ((count)[ch] as Int != 0) {
            return false
        }
    }
    return true
}

fun print_bool(b: Boolean): Unit {
    if ((b as Boolean)) {
        println(true)
    } else {
        println(false)
    }
}

fun main() {
    print_bool(check_anagrams("Silent", "Listen"))
    print_bool(check_anagrams("This is a string", "Is this a string"))
    print_bool(check_anagrams("This is    a      string", "Is     this a string"))
    print_bool(check_anagrams("There", "Their"))
}
