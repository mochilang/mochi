object csv_to_html_translation_2 {
  def main(args: Array[String]): Unit = {
    val c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!"
    var rows: List[List[String]] = scala.collection.mutable.ArrayBuffer[Any]()
    for(line <- split(c, "\n")) {
      rows = rows :+ split(line, ",")
    }
    val headings = true
    println("<table>")
    if (headings) {
      if (rows.length > 0) {
        var th = ""
        for(h <- (rows).apply(0)) {
          th = th + "<th>" + h + "</th>"
        }
        println("   <thead>")
        println("      <tr>" + th + "</tr>")
        println("   </thead>")
        println("   <tbody>")
        var i = 1
        while (i < rows.length) {
          var cells = ""
          for(cell <- (rows).apply(i)) {
            cells = cells + "<td>" + cell + "</td>"
          }
          println("      <tr>" + cells + "</tr>")
          i += 1
        }
        println("   </tbody>")
      }
    } else {
      for(row <- rows) {
        var cells = ""
        for(cell <- row) {
          cells = cells + "<td>" + cell + "</td>"
        }
        println("    <tr>" + cells + "</tr>")
      }
    }
    println("</table>")
  }
}
