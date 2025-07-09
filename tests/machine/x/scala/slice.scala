object slice {
  def main(args: Array[String]): Unit = {
    println((List(1, 2, 3).slice(1, 3)))
    println((List(1, 2, 3).slice(0, 2)))
    println(("hello".substring(1, 4)))
  }
}
