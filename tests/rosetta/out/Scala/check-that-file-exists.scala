object check_that_file_exists {
  def printStat(fs: Map[String, Boolean], path: String) = {
    if (fs.contains(path)) {
      if ((fs).apply(path)) {
        println(path + " is a directory")
      } else {
        println(path + " is a file")
      }
    } else {
      println(("stat " + path).asInstanceOf[Int] + ": no such file or directory")
    }
  }
  
  def main() = {
    var fs: Map[String, Boolean] = scala.collection.mutable.Map()
    fs("docs") = true
    for(p <- List("input.txt", "/input.txt", "docs", "/docs")) {
      printStat(fs, p)
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
