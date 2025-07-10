object map_assign {
  def main(args: Array[String]): Unit = {
    var scores = scala.collection.mutable.Map[String, Int]("alice" -> (1))
    scores("bob") = 2
    println((scores("bob")))
  }
}
