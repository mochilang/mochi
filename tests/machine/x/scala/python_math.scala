object python_math {
  def main(args: Array[String]): Unit = {
    val r = 3
    val area = scala.math.Pi * scala.math.pow(r, 2)
    val root = scala.math.sqrt(49)
    val sin45 = scala.math.sin(scala.math.Pi / 4)
    val log_e = scala.math.log(scala.math.E)
    println("Circle area with r =" + " " + r + " " + "=>" + " " + area)
    println("Square root of 49:" + " " + root)
    println("sin(π/4):" + " " + sin45)
    println("log(e):" + " " + log_e)
  }
}
