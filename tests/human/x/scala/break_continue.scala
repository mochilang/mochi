object break_continue {
  def main(args: Array[String]): Unit = {
    val numbers = List(1,2,3,4,5,6,7,8,9)
    for(n <- numbers) {
      if(n % 2 == 0) {
        // continue
      } else if(n > 7) {
        // break
        return
      } else {
        println(s"odd number: $n")
      }
    }
  }
}
