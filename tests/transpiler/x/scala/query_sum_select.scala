// Generated by Mochi v0.10.36 on 2025-07-22 17:43:31 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
object Main {
  val nums: ArrayBuffer[Int] = ArrayBuffer(1, 2, 3)
  
  val result = (for (n <- nums if (n > 1)) yield n).sum
  
  def main(args: Array[String]): Unit = {
    println(result)
  }
}
