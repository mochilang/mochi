object checkpoint_synchronization_4 {
  def main(args: Array[String]): Unit = {
    var nMech = 5
    var detailsPerMech = 4
    for(mech <- 1 until (nMech + 1)) {
      val id = mech
      println(((("worker " + id.toString).asInstanceOf[Int] + " contracted to assemble ").asInstanceOf[Int] + detailsPerMech.toString).asInstanceOf[Int] + " details")
      println(("worker " + id.toString).asInstanceOf[Int] + " enters shop")
      var d = 0
      while (d < detailsPerMech) {
        println(("worker " + id.toString).asInstanceOf[Int] + " assembling")
        println(("worker " + id.toString).asInstanceOf[Int] + " completed detail")
        d += 1
      }
      println(("worker " + id.toString).asInstanceOf[Int] + " leaves shop")
      println(("mechanism " + mech.toString).asInstanceOf[Int] + " completed")
    }
  }
}
