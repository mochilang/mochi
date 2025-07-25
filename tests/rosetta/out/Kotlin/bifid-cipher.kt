// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}
// Code generated from bifid-cipher.mochi

/**
 * Auto-generated from Mochi
 * @param square MutableList<MutableList<String>>
 * @return MutableMap<String, Any>
 */
fun square_to_maps(square: MutableList<MutableList<String>>): MutableMap<String, Any> {
    var emap: MutableMap<String, MutableList<Int>> = mutableMapOf<String, MutableList<Int>>()
    var dmap: MutableMap<String, String> = mutableMapOf<String, String>()
    var x = 0
    while (x < square.size) {
        val row = square[x]
        var y = 0
        while (y < row.size) {
            val ch = row[y]
            emap[ch] = mutableListOf(x, y)
            dmap[x.toString() + "," + y.toString()] = ch
            y = y + 1
        }
        x = x + 1
    }
    return mutableMapOf("e" to emap, "d" to dmap)
}

/**
 * Auto-generated from Mochi
 * @param text String
 * @param emap MutableMap<String, MutableList<Int>>
 * @return String
 */
fun remove_space(text: String, emap: MutableMap<String, MutableList<Int>>): String {
    val s = upper(text)
    var out = ""
    var i = 0
    while (i < s.length) {
        val ch = s.substring(i, i + 1)
        if (ch != " " && ch in emap) {
            out = out + ch
        }
        i = i + 1
    }
    return out
}

/**
 * Auto-generated from Mochi
 * @param text String
 * @param emap MutableMap<String, MutableList<Int>>
 * @param dmap MutableMap<String, String>
 * @return String
 */
fun encrypt(text: String, emap: MutableMap<String, MutableList<Int>>, dmap: MutableMap<String, String>): String {
    text = remove_space(text, emap)
    var row0: MutableList<Int> = mutableListOf<Int>()
    var row1: MutableList<Int> = mutableListOf<Int>()
    var i = 0
    while (i < text.length) {
        val ch = text.substring(i, i + 1)
        val xy = emap[ch]
        row0 = append(row0, xy[0])
        row1 = append(row1, xy[1])
        i = i + 1
    }
    for (v in row1) {
        row0 = append(row0, v)
    }
    var res = ""
    var j = 0
    while (j < row0.size) {
        val key = row0[j].toString() + "," + row0[j + 1].toString()
        res = res + dmap[key]
        j = j + 2
    }
    return res
}

/**
 * Auto-generated from Mochi
 * @param text String
 * @param emap MutableMap<String, MutableList<Int>>
 * @param dmap MutableMap<String, String>
 * @return String
 */
fun decrypt(text: String, emap: MutableMap<String, MutableList<Int>>, dmap: MutableMap<String, String>): String {
    text = remove_space(text, emap)
    var coords: MutableList<Int> = mutableListOf<Int>()
    var i = 0
    while (i < text.length) {
        val ch = text.substring(i, i + 1)
        val xy = emap[ch]
        coords = append(coords, xy[0])
        coords = append(coords, xy[1])
        i = i + 1
    }
    var half = (coords.size).toDouble() / (2).toDouble()
    var k1: MutableList<Int> = mutableListOf<Int>()
    var k2: MutableList<Int> = mutableListOf<Int>()
    var idx = 0
    while (idx < half) {
        k1 = append(k1, coords[idx])
        idx = idx + 1
    }
    while (idx < coords.size) {
        k2 = append(k2, coords[idx])
        idx = idx + 1
    }
    var res = ""
    var j = 0
    while (j < half) {
        val key = k1[j].toString() + "," + k2[j].toString()
        res = res + dmap[key]
        j = j + 1
    }
    return res
}

/**
 * Auto-generated from Mochi
 */
fun main(): Unit {
    val squareRosetta = mutableListOf(mutableListOf("A", "B", "C", "D", "E"), mutableListOf("F", "G", "H", "I", "K"), mutableListOf("L", "M", "N", "O", "P"), mutableListOf("Q", "R", "S", "T", "U"), mutableListOf("V", "W", "X", "Y", "Z"), mutableListOf("J", "1", "2", "3", "4"))
    val squareWikipedia = mutableListOf(mutableListOf("B", "G", "W", "K", "Z"), mutableListOf("Q", "P", "N", "D", "S"), mutableListOf("I", "O", "A", "X", "E"), mutableListOf("F", "C", "L", "U", "M"), mutableListOf("T", "H", "Y", "V", "R"), mutableListOf("J", "1", "2", "3", "4"))
    val textRosetta = "0ATTACKATDAWN"
    val textWikipedia = "FLEEATONCE"
    val textTest = "The invasion will start on the first of January"
    var maps = square_to_maps(squareRosetta)
    var emap = maps["e"]
    var dmap = maps["d"]
    println("from Rosettacode")
    println("original:\t " + textRosetta)
    var s = encrypt(textRosetta, emap, dmap)
    println("codiert:\t " + s)
    s = decrypt(s, emap, dmap)
    println("and back:\t " + s)
    maps = square_to_maps(squareWikipedia)
    emap = maps["e"]
    dmap = maps["d"]
    println("from Wikipedia")
    println("original:\t " + textWikipedia)
    s = encrypt(textWikipedia, emap, dmap)
    println("codiert:\t " + s)
    s = decrypt(s, emap, dmap)
    println("and back:\t " + s)
    maps = square_to_maps(squareWikipedia)
    emap = maps["e"]
    dmap = maps["d"]
    println("from Rosettacode long part")
    println("original:\t " + textTest)
    s = encrypt(textTest, emap, dmap)
    println("codiert:\t " + s)
    s = decrypt(s, emap, dmap)
    println("and back:\t " + s)
}

