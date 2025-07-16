object checkpoint_synchronization_2 {
  def lower(ch: String): String = {
    val upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val lower = "abcdefghijklmnopqrstuvwxyz"
    var i = 0
    while (i < upper.length) {
      if (ch == upper.substring(i, i + 1)) {
        return lower.substring(i, i + 1)
      }
      i += 1
    }
    return ch
  }
  
  def main(args: Array[String]): Unit = {
    var partList = scala.collection.mutable.ArrayBuffer("A", "B", "C", "D")
    var nAssemblies = 3
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
  }
}
