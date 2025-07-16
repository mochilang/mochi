object call_a_function_6 {
  def bar(a: Int, b: Int, c: Int) = {
    println((((a.toString + ", ").asInstanceOf[Int] + b.toString).asInstanceOf[Int] + ", ").asInstanceOf[Int] + c.toString)
  }
  
  def main() = {
    var args: Map[String, Int] = scala.collection.mutable.Map()
    args("a") = 3
    args("b") = 2
    args("c") = 1
    bar((args).apply("a"), (args).apply("b"), (args).apply("c"))
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
