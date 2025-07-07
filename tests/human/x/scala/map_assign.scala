object map_assign {
  def main(args: Array[String]): Unit = {
    val scores = scala.collection.mutable.Map("alice" -> 1)
    scores("bob") = 2
    println(scores("bob"))
  }
}
