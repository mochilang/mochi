object dataset_where_filter {
  val people = List(Map("name" -> ("Alice"), "age" -> (30)), Map("name" -> ("Bob"), "age" -> (15)), Map("name" -> ("Charlie"), "age" -> (65)), Map("name" -> ("Diana"), "age" -> (45)))
  val adults = for { person <- people; if (person("age")).asInstanceOf[Int] >= 18 } yield Map("name" -> (person("name")), "age" -> (person("age")), "is_senior" -> ((person("age")).asInstanceOf[Int] >= 60))
  def main(args: Array[String]): Unit = {
    println(("--- Adults ---"))
    for(person <- adults) {
      println((person("name")) + " " + ("is") + " " + (person("age")) + " " + (if ((person("is_senior")).asInstanceOf[Boolean]) " (senior)" else ""))
    }
  }
}
