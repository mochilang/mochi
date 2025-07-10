object group_by_having {
  val people = List[Map[String, String]](Map[String, String]("name" -> ("Alice"), "city" -> ("Paris")), Map[String, String]("name" -> ("Bob"), "city" -> ("Hanoi")), Map[String, String]("name" -> ("Charlie"), "city" -> ("Paris")), Map[String, String]("name" -> ("Diana"), "city" -> ("Hanoi")), Map[String, String]("name" -> ("Eve"), "city" -> ("Paris")), Map[String, String]("name" -> ("Frank"), "city" -> ("Hanoi")), Map[String, String]("name" -> ("George"), "city" -> ("Paris")))
  val big = (((for { p <- people } yield (p("city"), (p))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).filter{ case(gKey,gItems) => { val g = (gKey, gItems); (g._2).size >= 4 } }).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map[String, Any]("city" -> (g._1), "num" -> ((g._2).size)) } }.toList
  def main(args: Array[String]): Unit = {
    println(scala.util.parsing.json.JSONObject(big).toString())
  }
}
