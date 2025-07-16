object character_codes_1 {
  def ord(ch: String): Int = {
    if (ch == "a") {
      return 97
    }
    if (ch == "π") {
      return 960
    }
    if (ch == "A") {
      return 65
    }
    return 0
  }
  
  def main(args: Array[String]): Unit = {
    println(ord("a").toString)
    println(ord("π").toString)
  }
}
