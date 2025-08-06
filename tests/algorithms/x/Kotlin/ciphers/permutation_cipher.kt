import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var seed: Int = 1
var message: String = "HELLO WORLD"
var block_size: Int = generate_valid_block_size(message.length)
var key: MutableList<Int> = generate_permutation_key(block_size)
var encrypted: String = encrypt(message, key, block_size)
var decrypted: String = decrypt(encrypted, key)
fun rand(max: Int): Int {
    seed = Math.floorMod(((seed * 1103515245) + 12345), 2147483647)
    return Math.floorMod(seed, max)
}

fun generate_valid_block_size(message_length: Int): Int {
    var factors: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 2
    while (i <= message_length) {
        if ((Math.floorMod(message_length, i)) == 0) {
            factors = run { val _tmp = factors.toMutableList(); _tmp.add(i); _tmp }
        }
        i = i + 1
    }
    var idx: Int = rand(factors.size)
    return factors[idx]!!
}

fun generate_permutation_key(block_size: Int): MutableList<Int> {
    var digits: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < block_size) {
        digits = run { val _tmp = digits.toMutableList(); _tmp.add(i); _tmp }
        i = i + 1
    }
    var j: BigInteger = ((block_size - 1).toBigInteger())
    while (j.compareTo((0).toBigInteger()) > 0) {
        var k: Int = rand(((j.add((1).toBigInteger())).toInt()))
        var temp: Int = digits[(j).toInt()]!!
        _listSet(digits, (j).toInt(), digits[k]!!)
        _listSet(digits, k, temp)
        j = j.subtract((1).toBigInteger())
    }
    return digits
}

fun encrypt(message: String, key: MutableList<Int>, block_size: Int): String {
    var encrypted: String = ""
    var i: Int = 0
    while (i < message.length) {
        var block: String = message.substring(i, i + block_size)
        var j: Int = 0
        while (j < block_size) {
            encrypted = encrypted + block.substring(key[j]!!, key[j]!! + 1)
            j = j + 1
        }
        i = i + block_size
    }
    return encrypted
}

fun repeat_string(times: Int): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < times) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(""); _tmp }
        i = i + 1
    }
    return res
}

fun decrypt(encrypted: String, key: MutableList<Int>): String {
    var klen: Int = key.size
    var decrypted: String = ""
    var i: Int = 0
    while (i < encrypted.length) {
        var block: String = encrypted.substring(i, i + klen)
        var original: MutableList<String> = repeat_string(klen)
        var j: Int = 0
        while (j < klen) {
            _listSet(original, key[j]!!, block.substring(j, j + 1))
            j = j + 1
        }
        j = 0
        while (j < klen) {
            decrypted = decrypted + original[j]!!
            j = j + 1
        }
        i = i + klen
    }
    return decrypted
}

fun main() {
    println("Block size: " + block_size.toString())
    println("Key: " + key.toString())
    println("Encrypted: " + encrypted)
    println("Decrypted: " + decrypted)
}
