object group_by_having {
  case class Big(p: People)
  case class Big1(city: String, num: Int)
  case class People(name: String, city: String)

  val people = List[People](People(name = "Alice", city = "Paris"), People(name = "Bob", city = "Hanoi"), People(name = "Charlie", city = "Paris"), People(name = "Diana", city = "Hanoi"), People(name = "Eve", city = "Paris"), People(name = "Frank", city = "Hanoi"), People(name = "George", city = "Paris"))
  val big = (((for { p <- people } yield (p.city, Big(p = p))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).filter{ case(gKey,gItems) => { val g = (gKey, gItems); (g._2).size >= 4 } }).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map[String, Any]("city" -> (g.key), "num" -> ((g._2).size)) } }.toList
  def main(args: Array[String]): Unit = {
    println(scala.util.parsing.json.JSONObject(big).toString())
  }
}
