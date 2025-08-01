// Generated by Mochi v0.10.50 on 2025-07-31 07:52:53 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.math.BigInt
import scala.util.control.Breaks
import scala.util.control.Breaks._
object Main {
  def bubbleSort(a: ArrayBuffer[BigInt]): ArrayBuffer[BigInt] = {
    var arr: ArrayBuffer[BigInt] = a
    var itemCount: BigInt = (arr).size - BigInt(1)
    val _br0 = new Breaks
    _br0.breakable {
      while (true) {
        var hasChanged: Boolean = false
        var index: BigInt = BigInt(0)
        while (index < itemCount) {
          if (arr((index).toInt) > arr((index + BigInt(1)).toInt)) {
            val tmp: BigInt = arr((index).toInt)
            arr((index).toInt) = arr((index + BigInt(1)).toInt)
            arr((index + BigInt(1)).toInt) = tmp
            hasChanged = true
          }
          index = index + BigInt(1)
        }
        if ((!hasChanged).asInstanceOf[Boolean]) {
          _br0.break()
        }
        itemCount = itemCount - BigInt(1)
      }
    }
    return arr
  }
  
  var list: ArrayBuffer[BigInt] = ArrayBuffer(BigInt(31), BigInt(41), BigInt(59), BigInt(26), BigInt(53), BigInt(58), BigInt(97), BigInt(93), BigInt(23), BigInt(84))
  
  def main(args: Array[String]): Unit = {
    println("unsorted: " + String.valueOf(list))
    list = bubbleSort(list)
    println("sorted!  " + String.valueOf(list))
  }
}
