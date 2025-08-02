import java.math.BigInteger

fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

fun lshift(x: Int, n: Int): Int {
return (x.toLong() * pow2(n)).toInt()
}

fun rshift(x: Int, n: Int): Int {
return (x.toLong() / pow2(n)).toInt()
}

data class Writer(var order: String = "", var bits: Int = 0, var nbits: Int = 0, var data: MutableList<Int> = mutableListOf<Int>())
data class Reader(var order: String = "", var data: MutableList<Int> = mutableListOf<Int>(), var idx: Int = 0, var bits: Int = 0, var nbits: Int = 0)
fun NewWriter(order: String): Writer {
    return Writer(order = order, bits = 0, nbits = 0, data = mutableListOf<Int>())
}

fun writeBitsLSB(w: Writer, c: Int, width: Int): Writer {
    w.bits = w.bits + lshift(c, w.nbits)
    w.nbits = w.nbits + width
    while (w.nbits >= 8) {
        var b: BigInteger = (Math.floorMod(w.bits, 256)).toBigInteger()
        w.data = run { val _tmp = (w.data).toMutableList(); _tmp.add((b.toInt())); _tmp }
        w.bits = rshift(w.bits, 8)
        w.nbits = w.nbits - 8
    }
    return w
}

fun writeBitsMSB(w: Writer, c: Int, width: Int): Writer {
    w.bits = w.bits + lshift(c, (32 - width) - w.nbits)
    w.nbits = w.nbits + width
    while (w.nbits >= 8) {
        var b: BigInteger = (Math.floorMod(rshift(w.bits, 24), 256)).toBigInteger()
        w.data = run { val _tmp = (w.data).toMutableList(); _tmp.add((b.toInt())); _tmp }
        w.bits = (Math.floorMod((w.bits).toLong(), pow2(24)).toInt()) * 256
        w.nbits = w.nbits - 8
    }
    return w
}

fun WriteBits(w: Writer, c: Int, width: Int): Writer {
    if (w.order == "LSB") {
        return writeBitsLSB(w, c, width)
    }
    return writeBitsMSB(w, c, width)
}

fun CloseWriter(w: Writer): Writer {
    if (w.nbits > 0) {
        if (w.order == "MSB") {
            w.bits = rshift(w.bits, 24)
        }
        w.data = run { val _tmp = (w.data).toMutableList(); _tmp.add(Math.floorMod(w.bits, 256)); _tmp }
    }
    w.bits = 0
    w.nbits = 0
    return w
}

fun NewReader(data: MutableList<Int>, order: String): Reader {
    return Reader(order = order, data = data, idx = 0, bits = 0, nbits = 0)
}

fun readBitsLSB(r: Reader, width: Int): MutableMap<String, Any?> {
    while (r.nbits < width) {
        if (r.idx >= (r.data).size) {
            return mutableMapOf<String, Any?>("val" to (0), "eof" to (true))
        }
        var b: Int = (r.data)[r.idx]!!
        r.idx = r.idx + 1
        r.bits = r.bits + lshift(b, r.nbits)
        r.nbits = r.nbits + 8
    }
    var mask: BigInteger = (pow2(width) - (1).toLong()).toBigInteger()
    var out: BigInteger = (r.bits).toBigInteger().remainder((mask.add((1).toBigInteger())))
    r.bits = rshift(r.bits, width)
    r.nbits = r.nbits - width
    return mutableMapOf<String, Any?>("val" to (out), "eof" to (false))
}

fun readBitsMSB(r: Reader, width: Int): MutableMap<String, Any?> {
    while (r.nbits < width) {
        if (r.idx >= (r.data).size) {
            return mutableMapOf<String, Any?>("val" to (0), "eof" to (true))
        }
        var b: Int = (r.data)[r.idx]!!
        r.idx = r.idx + 1
        r.bits = r.bits + lshift(b, 24 - r.nbits)
        r.nbits = r.nbits + 8
    }
    var out: Int = rshift(r.bits, 32 - width)
    r.bits = Math.floorMod((((r.bits).toLong() * pow2(width)).toLong()), pow2(32)).toInt()
    r.nbits = r.nbits - width
    return mutableMapOf<String, Any?>("val" to (out), "eof" to (false))
}

fun ReadBits(r: Reader, width: Int): MutableMap<String, Any?> {
    if (r.order == "LSB") {
        return readBitsLSB(r, width)
    }
    return readBitsMSB(r, width)
}

fun toBinary(n: Int, bits: Int): String {
    var b: String = ""
    var _val: Int = n
    var i: Int = 0
    while (i < bits) {
        b = (Math.floorMod(_val, 2)).toString() + b
        _val = _val / 2
        i = i + 1
    }
    return b
}

fun bytesToBits(bs: MutableList<Int>): String {
    var out: String = "["
    var i: Int = 0
    while (i < bs.size) {
        out = out + toBinary(bs[i]!!, 8)
        if ((i + 1) < bs.size) {
            out = out + " "
        }
        i = i + 1
    }
    out = out + "]"
    return out
}

fun bytesToHex(bs: MutableList<Int>): String {
    var digits: String = "0123456789ABCDEF"
    var out: String = ""
    var i: Int = 0
    while (i < bs.size) {
        var b: Int = bs[i]!!
        var hi: Int = b / 16
        var lo: BigInteger = (Math.floorMod(b, 16)).toBigInteger()
        out = (out + digits.substring(hi, hi + 1)) + digits.substring((lo).toInt(), (lo.add((1).toBigInteger())).toInt())
        if ((i + 1) < bs.size) {
            out = out + " "
        }
        i = i + 1
    }
    return out
}

fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var idx: Int = upper.indexOf(ch)
    if (idx >= 0) {
        return 65 + idx
    }
    idx = lower.indexOf(ch)
    if (idx >= 0) {
        return 97 + idx
    }
    if ((ch >= "0") && (ch <= "9")) {
        return 48 + Integer.parseInt(ch, 10)
    }
    if (ch == " ") {
        return 32
    }
    if (ch == ".") {
        return 46
    }
    return 0
}

fun chr(n: Int): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    if ((n >= 65) && (n < 91)) {
        return upper.substring(n - 65, n - 64)
    }
    if ((n >= 97) && (n < 123)) {
        return lower.substring(n - 97, n - 96)
    }
    if ((n >= 48) && (n < 58)) {
        var digits: String = "0123456789"
        return digits.substring(n - 48, n - 47)
    }
    if (n == 32) {
        return " "
    }
    if (n == 46) {
        return "."
    }
    return "?"
}

fun bytesOfStr(s: String): MutableList<Int> {
    var bs: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < s.length) {
        bs = run { val _tmp = bs.toMutableList(); _tmp.add(ord(s.substring(i, i + 1))); _tmp }
        i = i + 1
    }
    return bs
}

fun bytesToDec(bs: MutableList<Int>): String {
    var out: String = ""
    var i: Int = 0
    while (i < bs.size) {
        out = out + (bs[i]!!).toString()
        if ((i + 1) < bs.size) {
            out = out + " "
        }
        i = i + 1
    }
    return out
}

fun Example(): Unit {
    var message: String = "This is a test."
    var msgBytes: MutableList<Int> = bytesOfStr(message)
    println((("\"" + message) + "\" as bytes: ") + bytesToDec(msgBytes))
    println("    original bits: " + bytesToBits(msgBytes))
    var bw: Writer = NewWriter("MSB")
    var i: Int = 0
    while (i < msgBytes.size) {
        bw = WriteBits(bw, msgBytes[i]!!, 7)
        i = i + 1
    }
    bw = CloseWriter(bw)
    println("Written bitstream: " + bytesToBits(bw.data))
    println("Written bytes: " + bytesToHex(bw.data))
    var br: Reader = NewReader(bw.data, "MSB")
    var result: String = ""
    while (true) {
        var r: MutableMap<String, Any?> = ReadBits(br, 7)
        if ((((r)["eof"] as Any?) as Boolean)) {
            break
        }
        var v: Int = (r)["val"] as Int
        if (v != 0) {
            result = result + chr(v)
        }
    }
    println(("Read back as \"" + result) + "\"")
}

fun main() {
    Example()
}
