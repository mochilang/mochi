object call_a_function_4 {
  def gifEncode(out: any, img: any, opts: Map[String, Int]) = {
  }
  
  def main() = {
    var opts: Map[String, Int] = scala.collection.mutable.Map()
    opts("NumColors") = 16
    gifEncode(null, null, opts)
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
