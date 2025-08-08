import java.math.BigInteger

var sample: String = "1011001"
var decompressed: String = decompress_data(sample)
fun list_contains(xs: MutableList<String>, v: String): Boolean {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == v) {
            return true
        }
        i = i + 1
    }
    return false
}

fun is_power_of_two(n: Int): Boolean {
    if (n < 1) {
        return false
    }
    var x: Int = (n).toInt()
    while (x > 1) {
        if ((Math.floorMod(x, 2)) != 0) {
            return false
        }
        x = x / 2
    }
    return true
}

fun bin_string(n: Int): String {
    if (n == 0) {
        return "0"
    }
    var res: String = ""
    var x: Int = (n).toInt()
    while (x > 0) {
        var bit: Int = (Math.floorMod(x, 2)).toInt()
        res = bit.toString() + res
        x = x / 2
    }
    return res
}

fun decompress_data(data_bits: String): String {
    var lexicon: MutableMap<String, String> = (mutableMapOf<String, String>("0" to ("0"), "1" to ("1")) as MutableMap<String, String>)
    var keys: MutableList<String> = mutableListOf("0", "1")
    var result: String = ""
    var curr_string: String = ""
    var index: Int = (2).toInt()
    var i: Int = (0).toInt()
    while (i < data_bits.length) {
        curr_string = curr_string + data_bits.substring(i, i + 1)
        if (!list_contains(keys, curr_string)) {
            i = i + 1
            continue
        }
        var last_match_id: String = (lexicon)[curr_string] as String
        result = result + last_match_id
        (lexicon)[curr_string] = last_match_id + "0"
        if (((is_power_of_two(index)) as Boolean)) {
            var new_lex: MutableMap<String, String> = mutableMapOf<String, String>()
            var new_keys: MutableList<String> = mutableListOf<String>()
            var j: Int = (0).toInt()
            while (j < keys.size) {
                var curr_key: String = keys[j]!!
                (new_lex)["0" + curr_key] = (lexicon)[curr_key] as String
                new_keys = run { val _tmp = new_keys.toMutableList(); _tmp.add("0" + curr_key); _tmp }
                j = j + 1
            }
            lexicon = new_lex
            keys = new_keys
        }
        var new_key: String = bin_string(index)
        (lexicon)[new_key] = last_match_id + "1"
        keys = run { val _tmp = keys.toMutableList(); _tmp.add(new_key); _tmp }
        index = index + 1
        curr_string = ""
        i = i + 1
    }
    return result
}

fun main() {
    println(decompressed)
}
