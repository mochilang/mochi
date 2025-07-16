object csv_to_html_translation_5 {
  def split(s: String, sep: String): List[String] = {
    var out: List[String] = scala.collection.mutable.ArrayBuffer[Any]()
    var start = 0
    var i = 0
    val n = sep.length
    while (i <= s.length - n) {
      if (s.substring(i, i + n) == sep) {
        out = out :+ s.substring(start, i)
        i += n
        start = i
      } else {
        i += 1
      }
    }
    out = out :+ s.substring(start, s.length)
    return out
  }
  
  def htmlEscape(s: String): String = {
    var out = ""
    var i = 0
    while (i < s.length) {
      val ch = s.substring(i, i + 1)
      if (ch == "&") {
        out += "&amp;"
      } else {
        if (ch == "<") {
          out += "&lt;"
        } else {
          if (ch == ">") {
            out += "&gt;"
          } else {
            out += ch
          }
        }
      }
      i += 1
    }
    return out
  }
  
  def main(args: Array[String]): Unit = {
    val c = (((("Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n").asInstanceOf[Int] + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n").asInstanceOf[Int] + "The multitude,Who are you?\n").asInstanceOf[Int] + "Brians mother,I'm his mother; that's who!\n").asInstanceOf[Int] + "The multitude,Behold his mother! Behold his mother!"
    var rows: List[List[String]] = scala.collection.mutable.ArrayBuffer[Any]()
    for(line <- split(c, "\n")) {
      rows = rows :+ split(line, ",")
    }
    println("<table>")
    for(row <- rows) {
      var cells = ""
      for(cell <- row) {
        cells = ((cells + "<td>").asInstanceOf[Int] + htmlEscape(cell)).asInstanceOf[Int] + "</td>"
      }
      println(("    <tr>" + cells).asInstanceOf[Int] + "</tr>")
    }
    println("</table>")
  }
}
