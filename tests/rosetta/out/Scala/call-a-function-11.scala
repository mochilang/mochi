object call_a_function_11 {
  def zeroval(ival: Int): Int = {
    var x = ival
    x = 0
    return x
  }
  
  def zeroptr(ref: List[Int]) = {
    ref(0) = 0
  }
  
  def main() = {
    var i = 1
    println("initial: " + i.toString)
    val tmp = zeroval(i)
    println("zeroval: " + i.toString)
    var box = scala.collection.mutable.ArrayBuffer(i)
    zeroptr(box)
    i = (box).apply(0)
    println("zeroptr: " + i.toString)
    println("pointer: 0")
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
