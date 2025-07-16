object csv_to_html_translation_3 {
  def main(args: Array[String]): Unit = {
    println("<table>")
    println("   <thead>")
    println("      <tr><th>Character</th><th>Speech</th></tr>")
    println("   </thead>")
    println("   <tbody>")
    println("      <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>")
    println("      <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>")
    println("      <tr><td>The multitude</td><td>Who are you?</td></tr>")
    println("      <tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>")
    println("      <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>")
    println("   </tbody>")
    println("</table>")
  }
}
