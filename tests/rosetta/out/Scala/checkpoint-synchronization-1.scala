object checkpoint_synchronization_1 {
  def main(args: Array[String]): Unit = {
    var partList = scala.collection.mutable.ArrayBuffer("A", "B", "C", "D")
    var nAssemblies = 3
    for(cycle <- 1 until (nAssemblies + 1)) {
      println("begin assembly cycle " + cycle.toString)
      for(p <- partList) {
        println(p + " worker begins part")
      }
      for(p <- partList) {
        println(p + " worker completes part")
      }
      println("assemble.  cycle " + cycle.toString + " complete")
    }
  }
}
