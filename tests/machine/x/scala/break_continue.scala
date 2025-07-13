object break_continue {
  val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  def main(args: Array[String]): Unit = {
    for(n <- numbers) {
      if (n % 2 == 0) {
        // continue
      }
      if (n > 7) {
        return
      }
      println(s"odd number: ${n}")
    }
  }
}
