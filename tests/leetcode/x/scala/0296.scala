import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object Main {
  def minTotalDistance(grid: Array[Array[Int]]): Int = {
    val rows = ArrayBuffer.empty[Int]
    val cols = ArrayBuffer.empty[Int]
    for (i <- grid.indices; j <- grid(i).indices if grid(i)(j) == 1) rows += i
    for (j <- grid(0).indices; i <- grid.indices if grid(i)(j) == 1) cols += j
    val mr = rows(rows.length / 2)
    val mc = cols(cols.length / 2)
    rows.map(x => math.abs(x - mr)).sum + cols.map(x => math.abs(x - mc)).sum
  }

  def main(args: Array[String]): Unit = {
    val data = Iterator.continually(StdIn.readLine()).takeWhile(_ != null).flatMap(_.trim.split("\\s+")).filter(_.nonEmpty).map(_.toInt).toArray
    if (data.isEmpty) return
    var idx = 0
    val t = data(idx)
    idx += 1
    val out = ArrayBuffer.empty[String]
    for (_ <- 0 until t) {
      val r = data(idx)
      val c = data(idx + 1)
      idx += 2
      val grid = Array.ofDim[Int](r, c)
      for (i <- 0 until r; j <- 0 until c) {
        grid(i)(j) = data(idx)
        idx += 1
      }
      out += minTotalDistance(grid).toString
    }
    print(out.mkString("\n\n"))
  }
}
