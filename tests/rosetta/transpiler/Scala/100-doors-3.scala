// Generated by Mochi v0.10.40 on 2025-07-25 12:50:14 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
object Main {
  var result: String = ""
  
  def main(args: Array[String]): Unit = {
    for (i <- 1 until 101) {
      var j: Int = 1
      while ((j * j).asInstanceOf[Int] < i.asInstanceOf[Int]) {
        j = (j + 1).asInstanceOf[Int]
      }
      if (j * j == i) {
        result = result + "O"
      } else {
        result = result + "-"
      }
    }
    println(result)
  }
}
