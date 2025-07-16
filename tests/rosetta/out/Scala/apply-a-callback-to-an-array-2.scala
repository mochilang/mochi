object apply_a_callback_to_an_array_2 {
  def each(xs: List[Int], f: (Int) => Any) = {
    for(x <- xs) {
      f(x)
    }
  }
  
  def Map(xs: List[Int], f: (Int) => Int): List[Int] = {
    var r: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    for(x <- xs) {
      r = r :+ f(x)
    }
    return r
  }
  
  def main() = {
    val s: List[Int] = List(1, 2, 3, 4, 5)
    each(s, (i: Int) => print(i * i.toString))
    println(Map(s, (i: Int) => i * i).toString)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
