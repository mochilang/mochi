import scala.io.Source

object Main extends App {
  val tokens = Source.stdin.mkString.split("\\s+").filter(_.nonEmpty)
  if (tokens.nonEmpty) {
    var idx = 0
    val t = tokens(idx).toInt
    idx += 1
    def lcp(strs: Array[String]): String = {
      var prefix = strs(0)
      while (!strs.forall(_.startsWith(prefix))) prefix = prefix.dropRight(1)
      prefix
    }
    val out = collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val n = tokens(idx).toInt; idx += 1
      val strs = tokens.slice(idx, idx + n); idx += n
      out += s""""${lcp(strs)}""""
    }
    print(out.mkString("\n"))
  }
}
