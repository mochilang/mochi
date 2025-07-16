object arithmetic_integer_1 {
  def main() = {
    val a = 12
    val b = 8
    println((((a.toString + " + ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + a + b.toString)
    println((((a.toString + " - ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + a - b.toString)
    println((((a.toString + " * ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + a * b.toString)
    println((((a.toString + " / ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + (a / b).toInt.toString)
    println((((a.toString + " % ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + " = ").asInstanceOf[Int] + a % b.toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
