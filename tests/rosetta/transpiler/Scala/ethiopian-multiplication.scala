// Generated by Mochi v0.10.52 on 2025-08-02 01:16:37 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.math.BigInt
object Main {
  def halve(i: BigInt): BigInt = {
    return i / BigInt(2)
  }
  
  def double(i: BigInt): BigInt = {
    return i * BigInt(2)
  }
  
  def isEven(i: BigInt): Boolean = {
    return i % BigInt(2) == BigInt(0)
  }
  
  def ethMulti(i: BigInt, j: BigInt): BigInt = {
    var r: BigInt = BigInt(0)
    var x: BigInt = i
    var y: BigInt = j
    while (x > BigInt(0)) {
      if ((!isEven(x)).asInstanceOf[Boolean]) {
        r = r + y
      }
      x = halve(x)
      y = double(y)
    }
    return r
  }
  
  def main(args: Array[String]): Unit = {
    println("17 ethiopian 34 = " + String.valueOf(ethMulti(BigInt(17), BigInt(34))))
  }
}
