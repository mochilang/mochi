object if_then_else_nested {
  def main(args: Array[String]): Unit = {
    val x = 8
    val msg = if(x > 10) "big" else if(x > 5) "medium" else "small"
    println(msg)
  }
}

