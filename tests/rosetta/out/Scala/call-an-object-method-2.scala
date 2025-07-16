case class Box(var Contents: String, var secret: Int)

object call_an_object_method_2 {
  def newFactory(): List[() => Any] = {
    var sn = 0
    def New(): Box = {
      sn += 1
      var b = Box(secret = sn)
      if (sn == 1) {
        b.Contents = "rabbit"
      } else {
        if (sn == 2) {
          b.Contents = "rock"
        }
      }
      return b
    }
    def Count(): Int = sn
    return List(New, Count)
  }
  
  def main(args: Array[String]): Unit = {
    val funcs = newFactory()
    val New = (funcs).apply(0)
    val Count = (funcs).apply(1)
  }
}
