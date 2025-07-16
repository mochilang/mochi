object _100_doors_2 {
  def main(args: Array[String]): Unit = {
    var door = 1
    var incrementer = 0
    for(current <- 1 until 101) {
      var line = "Door " + current.toString + " "
      if (current == door) {
        line += "Open"
        incrementer += 1
        door = door + 2 * incrementer + 1
      } else {
        line += "Closed"
      }
      println(line)
    }
  }
}
