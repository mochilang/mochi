object Main {
  case class Row(dept: String, name: String, salary: Int)

  def main(args: Array[String]): Unit = {
    val toks = io.Source.stdin.getLines().flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).toArray
    if (toks.isEmpty) return
    var idx = 0
    val t = toks(idx).toInt
    idx += 1
    val cases = new scala.collection.mutable.ArrayBuffer[String]()
    for (_ <- 0 until t) {
      val d = toks(idx).toInt
      val e = toks(idx + 1).toInt
      idx += 2
      val deptName = scala.collection.mutable.Map[Int, String]()
      for (_ <- 0 until d) { deptName(toks(idx).toInt) = toks(idx + 1); idx += 2 }
      val groups = scala.collection.mutable.Map[Int, scala.collection.mutable.ArrayBuffer[(String, Int)]]()
      for (_ <- 0 until e) {
        idx += 1
        val name = toks(idx)
        val salary = toks(idx + 1).toInt
        val deptId = toks(idx + 2).toInt
        idx += 3
        groups.getOrElseUpdate(deptId, scala.collection.mutable.ArrayBuffer()) += ((name, salary))
      }
      val rows = scala.collection.mutable.ArrayBuffer[Row]()
      for ((deptId, items) <- groups) {
        val keep = items.map(_._2).distinct.sorted(Ordering.Int.reverse).take(3).toSet
        for ((name, salary) <- items if keep.contains(salary)) rows += Row(deptName(deptId), name, salary)
      }
      val sorted = rows.sortBy(r => (r.dept, -r.salary, r.name))
      cases += ((Seq(sorted.size.toString) ++ sorted.map(r => s"${r.dept},${r.name},${r.salary}")).mkString("\n"))
    }
    print(cases.mkString("\n\n"))
  }
}
