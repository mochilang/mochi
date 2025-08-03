import java.math.BigInteger

data class Info(var animal: String = "", var yinYang: String = "", var element: String = "", var stemBranch: String = "", var cycle: Int = 0)
var animal: MutableList<String> = mutableListOf("Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig")
var yinYang: MutableList<String> = mutableListOf("Yang", "Yin")
var element: MutableList<String> = mutableListOf("Wood", "Fire", "Earth", "Metal", "Water")
var stemChArr: MutableList<String> = mutableListOf("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸")
var branchChArr: MutableList<String> = mutableListOf("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥")
fun cz(yr: Int, animal: MutableList<String>, yinYang: MutableList<String>, element: MutableList<String>, sc: MutableList<String>, bc: MutableList<String>): Info {
    var y: BigInteger = (yr - 4).toBigInteger()
    var stem: BigInteger = y.remainder((10).toBigInteger())
    var branch: BigInteger = y.remainder((12).toBigInteger())
    var sb: String = sc[(stem).toInt()]!! + bc[(branch).toInt()]!!
    return Info(animal = ((animal[(branch).toInt()]!!).toString()), yinYang = ((yinYang[(stem.remainder((2).toBigInteger())).toInt()]!!).toString()), element = ((element[((stem.divide((2).toBigInteger())).toInt())]!!).toString()), stemBranch = sb, cycle = (((y.remainder((60).toBigInteger())).add((1).toBigInteger())).toInt()))
}

fun main() {
    for (yr in mutableListOf(1935, 1938, 1968, 1972, 1976)) {
        var r: Info = cz(yr, animal, yinYang, element, stemChArr, branchChArr)
        println((((((((((yr.toString() + ": ") + r.element) + " ") + r.animal) + ", ") + r.yinYang) + ", Cycle year ") + r.cycle.toString()) + " ") + r.stemBranch)
    }
}
