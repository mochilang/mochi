import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var seed: Int = 123456789
var PRIME: Int = 23
var generator: Int = 5
var alice_private: Int = generate_private_key()
var alice_public: Int = mod_pow(generator, alice_private)
var bob_private: Int = generate_private_key()
var bob_public: Int = mod_pow(generator, bob_private)
var alice_shared: Int = mod_pow(bob_public, alice_private)
var bob_shared: Int = mod_pow(alice_public, bob_private)
fun int_to_hex(n: Int): String {
    if (n == 0) {
        return "0"
    }
    var digits: String = "0123456789abcdef"
    var num: Int = n
    var res: String = ""
    while (num > 0) {
        var d: Int = Math.floorMod(num, 16)
        res = digits[d].toString() + res
        num = num / 16
    }
    return res
}

fun rand_int(): Int {
    seed = ((Math.floorMod((((1103515245 * seed) + 12345).toLong()), 2147483648L)).toInt())
    return seed
}

fun mod_pow(base: Int, exp: Int): Int {
    var result: Int = 1
    var b: BigInteger = ((Math.floorMod(base, PRIME)).toBigInteger())
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = ((((result).toBigInteger().multiply((b))).remainder((PRIME).toBigInteger())).toInt())
        }
        b = (b.multiply((b))).remainder((PRIME).toBigInteger())
        e = e / 2
    }
    return result
}

fun is_valid_public_key(key: Int): Boolean {
    if ((key < 2) || (key > (PRIME - 2))) {
        return false
    }
    return mod_pow(key, (PRIME - 1) / 2) == 1
}

fun generate_private_key(): Int {
    return (Math.floorMod(rand_int(), (PRIME - 2))) + 2
}

fun main() {
    if (!is_valid_public_key(alice_public)) {
        panic("Invalid public key")
    }
    if (!is_valid_public_key(bob_public)) {
        panic("Invalid public key")
    }
    println(int_to_hex(alice_shared))
    println(int_to_hex(bob_shared))
}
