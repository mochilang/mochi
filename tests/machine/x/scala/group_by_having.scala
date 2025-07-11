object group_by_having {
  case class Auto1(name: String, city: String)
  case class Auto2(city: String, num: Int)

  val people = List[Auto1](Auto1(name = "Alice", city = "Paris"), Auto1(name = "Bob", city = "Hanoi"), Auto1(name = "Charlie", city = "Paris"), Auto1(name = "Diana", city = "Hanoi"), Auto1(name = "Eve", city = "Paris"), Auto1(name = "Frank", city = "Hanoi"), Auto1(name = "George", city = "Paris"))
  val big = (((for { p <- people } yield (p.city, (p))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).filter{ case(gKey,gItems) => { val g = (gKey, gItems); (g._2).size >= 4 } }).map{ case(gKey,gItems) => { val g = (gKey, gItems); Auto2(city = g._1, num = (g._2).size) } }.toList
  def main(args: Array[String]): Unit = {
    println(scala.util.parsing.json.JSONObject(big).toString())
  }
}
