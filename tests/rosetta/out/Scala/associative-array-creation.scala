object associative_array_creation {
  def removeKey(m: Map[String, Int], k: String): Map[String, Int] = {
    var out: Map[String, Int] = scala.collection.mutable.Map()
    for((key, _) <- m) {
      if (key != k) {
        out(key) = (m).apply(key)
      }
    }
    return out
  }
  
  def main() = {
    var x: Map[String, Int] = null
    x = Map()
    x("foo") = 3
    val y1 = (x).apply("bar")
    val ok = x.contains("bar")
    println(y1)
    println(ok)
    x = removeKey(x, "foo")
    x = Map("foo" -> 2, "bar" -> 42, "baz" -> -1)
    println(s"${(x).apply("foo")} ${(x).apply("bar")} ${(x).apply("baz")}")
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
