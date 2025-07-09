object group_by {
  val people = List(Map("name" -> ("Alice"), "age" -> (30), "city" -> ("Paris")), Map("name" -> ("Bob"), "age" -> (15), "city" -> ("Hanoi")), Map("name" -> ("Charlie"), "age" -> (65), "city" -> ("Paris")), Map("name" -> ("Diana"), "age" -> (45), "city" -> ("Hanoi")), Map("name" -> ("Eve"), "age" -> (70), "city" -> ("Paris")), Map("name" -> ("Frank"), "age" -> (22), "city" -> ("Hanoi")))
  val stats = ((for { person <- people } yield (person("city"), (person))).groupBy(_._1).map{ case(k,list) => (k, list.map(_._2)) }.toList).map{ case(gKey,gItems) => { val g = (gKey, gItems); Map("city" -> (g._1), "count" -> ((g)._2.size), "avg_age" -> ((for { p <- g } yield p.age).sum.toDouble / (for { p <- g } yield p.age).size)) } }.toList
  def main(args: Array[String]): Unit = {
    println(("--- People grouped by city ---"))
    for(s <- stats) {
      println((s("city")) + " " + (": count =") + " " + (s("count")) + " " + (", avg_age =") + " " + (s("avg_age")))
    }
  }
}
