// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:57:14Z
var animal = ["Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"]
var yinYang = ["Yang", "Yin"]
var element = ["Wood", "Fire", "Earth", "Metal", "Water"]
var stemChArr = ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"]
var branchChArr = ["子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"]
struct Info: Equatable {
    var animal: String
    var yinYang: String
    var element: String
    var stemBranch: String
    var cycle: Int
}
func cz(_ yr: Int, _ animal: [String], _ yinYang: [String], _ element: [String], _ sc: [String], _ bc: [String]) -> Info {
    var y = yr - 4
    let stem = y % 10
    let branch = y % 12
    let sb = sc[stem] + bc[branch]
    return Info(animal: String(animal[branch]), yinYang: String(yinYang[stem % 2]), element: String(element[Int((stem / 2))]), stemBranch: sb, cycle: y % 60 + 1)
}
for yr in [1935, 1938, 1968, 1972, 1976] {
    let r = cz(yr, animal, yinYang, element, stemChArr, branchChArr)
    print(String(yr) + ": " + r.element + " " + r.animal + ", " + r.yinYang + ", Cycle year " + String(r.cycle) + " " + r.stemBranch)
}
