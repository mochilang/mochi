case class Info(var animal: String, var yinYang: String, var element: String, var stemBranch: String, var cycle: Int)

object chinese_zodiac {
  val animal = List("Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig")
  val yinYang = List("Yang", "Yin")
  val element = List("Wood", "Fire", "Earth", "Metal", "Water")
  val stemChArr = List("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸")
  val branchChArr = List("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥")
  def cz(yr: Int, animal: List[String], yinYang: List[String], element: List[String], sc: List[String], bc: List[String]): Info = {
    var y = yr - 4
    val stem = y % 10
    val branch = y % 12
    val sb = (sc).apply(stem) + (bc).apply(branch)
    return Info(animal = (animal).apply(branch).toString, yinYang = (yinYang).apply(stem % 2).toString, element = (element).apply((stem / 2).toInt).toString, stemBranch = sb, cycle = y % 60 + 1)
  }
  
  def main(args: Array[String]): Unit = {
    for(yr <- List(1935, 1938, 1968, 1972, 1976)) {
      val r = cz(yr, animal, yinYang, element, stemChArr, branchChArr)
      println((((((((((yr.toString + ": ").asInstanceOf[Int] + r.element).asInstanceOf[Int] + " ").asInstanceOf[Int] + r.animal).asInstanceOf[Int] + ", ").asInstanceOf[Int] + r.yinYang).asInstanceOf[Int] + ", Cycle year ").asInstanceOf[Int] + r.cycle.toString).asInstanceOf[Int] + " ").asInstanceOf[Int] + r.stemBranch)
    }
  }
}
