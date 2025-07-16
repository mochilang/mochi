object checkpoint_synchronization_3 {
  def lower(ch: String): String = {
    val up = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val low = "abcdefghijklmnopqrstuvwxyz"
    var i = 0
    while (i < up.length) {
      if (ch == up.substring(i, i + 1)) {
        return low.substring(i, i + 1)
      }
      i += 1
    }
    return ch
  }
  
  def main(args: Array[String]): Unit = {
    var partList = scala.collection.mutable.ArrayBuffer("A", "B", "C", "D")
    var nAssemblies = 3
    for(p <- partList) {
      println(p + " worker running")
    }
    for(cycle <- 1 until (nAssemblies + 1)) {
      println("begin assembly cycle " + cycle.toString)
      var a = ""
      for(p <- partList) {
        println(p + " worker begins part")
        println((p + " worker completed ").asInstanceOf[Int] + lower(p))
        a += lower(p)
      }
      println(((a + " assembled.  cycle ").asInstanceOf[Int] + cycle.toString).asInstanceOf[Int] + " complete")
    }
    for(p <- partList) {
      println(p + " worker stopped")
    }
  }
}
