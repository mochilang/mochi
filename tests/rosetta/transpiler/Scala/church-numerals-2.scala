// Generated by Mochi v0.10.41 on 2025-07-27 16:19:10 GMT+7
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.math.BigInt
object Main {
  type Church = (Church) => Church
  
  def id(x: Any): Any = {
    return x
  }
  
  def compose(f: (Any) => Any, g: (Any) => Any): (Any) => Any = {
    return ((x: Any) => f(g(x)))
  }
  
  def zero(): Church = {
    return (((f: Church) => id _)).asInstanceOf[Church]
  }
  
  def one(): Church = {
    return (id _).asInstanceOf[Church]
  }
  
  def succ(n: Church): Church = {
    return (((f: Church) => compose(f.asInstanceOf[(Any) => Any], (n(f)).asInstanceOf[(Any) => Any]))).asInstanceOf[Church]
  }
  
  def plus(m: Church, n: Church): Church = {
    return (((f: Church) => compose((m(f)).asInstanceOf[(Any) => Any], (n(f)).asInstanceOf[(Any) => Any]))).asInstanceOf[Church]
  }
  
  def mult(m: Church, n: Church): Church = {
    return (compose(m.asInstanceOf[(Any) => Any], n.asInstanceOf[(Any) => Any])).asInstanceOf[Church]
  }
  
  def exp(m: Church, n: Church): Church = {
    return (n(m)).asInstanceOf[Church]
  }
  
  def toInt(x: Church): BigInt = {
    var counter: BigInt = BigInt(0)
    def fCounter(f: Church): Church = {
      counter = (counter + BigInt(1)).asInstanceOf[BigInt]
      return f
    }
    x(fCounter)(id _)
    return counter
  }
  
  def toStr(x: Church): String = {
    var s: String = ""
    def fCounter(f: Church): Church = {
      s = s + "|"
      return f
    }
    x(fCounter)(id _)
    return s
  }
  
  def main(): Any = {
    println("zero = " + String.valueOf(toInt((zero()).asInstanceOf[Church])))
    val onev: (Any) => Any = one()
    println("one = " + String.valueOf(toInt(onev.asInstanceOf[Church])))
    val two: (Any) => Any = succ((succ((zero()).asInstanceOf[Church])).asInstanceOf[Church])
    println("two = " + String.valueOf(toInt(two.asInstanceOf[Church])))
    val three: (Any) => Any = plus(onev.asInstanceOf[Church], two.asInstanceOf[Church])
    println("three = " + String.valueOf(toInt(three.asInstanceOf[Church])))
    val four: (Any) => Any = mult(two.asInstanceOf[Church], two.asInstanceOf[Church])
    println("four = " + String.valueOf(toInt(four.asInstanceOf[Church])))
    val eight: (Any) => Any = exp(two.asInstanceOf[Church], three.asInstanceOf[Church])
    println("eight = " + String.valueOf(toInt(eight.asInstanceOf[Church])))
    println("toStr(four) = " + toStr(four.asInstanceOf[Church]))
  }
  
  def main(args: Array[String]): Unit = {
  }
}
