object csv_to_html_translation_1 {
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
        cells = ((cells + "<td>").asInstanceOf[Int] + cell).asInstanceOf[Int] + "</td>"
      }
      println(("    <tr>" + cells).asInstanceOf[Int] + "</tr>")
    }
    println("</table>")
  }
}
