object checkpoint_synchronization_4 {
  def main(args: Array[String]): Unit = {
    var nMech = 5
    var detailsPerMech = 4
    for(mech <- 1 until (nMech + 1)) {
      val id = mech
      println("worker " + id.toString + " contracted to assemble " + detailsPerMech.toString + " details")
      println("worker " + id.toString + " enters shop")
      var d = 0
      while (d < detailsPerMech) {
        println("worker " + id.toString + " assembling")
        println("worker " + id.toString + " completed detail")
        d += 1
      }
      println("worker " + id.toString + " leaves shop")
      println("mechanism " + mech.toString + " completed")
    }
  }
}
