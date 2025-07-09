object group_by_having {
  def main(args: Array[String]): Unit = {
    val people = List(Map("name" -> ("Alice"), "city" -> ("Paris")), Map("name" -> ("Bob"), "city" -> ("Hanoi")), Map("name" -> ("Charlie"), "city" -> ("Paris")), Map("name" -> ("Diana"), "city" -> ("Hanoi")), Map("name" -> ("Eve"), "city" -> ("Paris")), Map("name" -> ("Frank"), "city" -> ("Hanoi")), Map("name" -> ("George"), "city" -> ("Paris")))
    val big = (((for { p <- people } yield (p("city"), (p))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).filter{ case(gKey,gItems) => { val g = (gKey, gItems); (g)._2.size >= 4 } }).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map("city" -> (g._1), "num" -> ((g)._2.size)) } }.toList
    println(scala.util.parsing.json.JSONObject(big).toString())
  }
}
