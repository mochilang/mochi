object in_operator {
  val xs = List[Int](1, 2, 3)
  def main(args: Array[String]): Unit = {
    println((xs.contains(2)))
    println((!(xs.contains(5))))
  }
}
