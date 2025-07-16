case class Box(var Contents: String, var secret: Int)

object call_an_object_method {
  def New(): Box = {
    var b = Box(Contents = "rabbit", secret = 1)
    return b
  }
  
  def main(args: Array[String]): Unit = {
    var box = New()
    box.TellSecret()
  }
}
