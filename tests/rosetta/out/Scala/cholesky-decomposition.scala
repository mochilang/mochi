object cholesky_decomposition {
  def sqrtApprox(x: Double): Double = {
    var guess = x
    var i = 0
    while (i < 20) {
      guess = (guess + x / guess) / 2
      i += 1
    }
    return guess
  }
  
  def cholesky(a: List[List[Double]]): List[List[Double]] = {
    val n = a.length
    var l: List[List[Double]] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < n) {
      var row: List[Double] = scala.collection.mutable.ArrayBuffer[Any]()
      var j = 0
      while (j < n) {
        row = row :+ 0
        j += 1
      }
      l = l :+ row
      i += 1
    }
    i = 0
    while (i < n) {
      var j = 0
      while (j <= i) {
        var sum = ((a).apply(i)).apply(j)
        var k = 0
        while (k < j) {
          sum = sum - ((l).apply(i)).apply(k) * ((l).apply(j)).apply(k)
          k += 1
        }
        if (i == j) {
          val _tmp0 = l(i).updated(j, sqrtApprox(sum))
          l = l.updated(i, _tmp0)
        } else {
          val _tmp1 = l(i).updated(j, sum / ((l).apply(j)).apply(j))
          l = l.updated(i, _tmp1)
        }
        j += 1
      }
      i += 1
    }
    return l
  }
  
  def printMat(m: List[List[Double]]) = {
    var i = 0
    while (i < m.length) {
      var line = ""
      var j = 0
      while (j < (m).apply(i).length) {
        line += ((m).apply(i)).apply(j).toString
        if (j < (m).apply(i).length - 1) {
          line += " "
        }
        j += 1
      }
      println(line)
      i += 1
    }
  }
  
  def demo(a: List[List[Double]]) = {
    println("A:")
    printMat(a)
    val l = cholesky(a)
    println("L:")
    printMat(l)
  }
  
  def main(args: Array[String]): Unit = {
    demo(List(List(25, 15, -5), List(15, 18, 0), List(-5, 0, 11)))
    demo(List(List(18, 22, 54, 42), List(22, 70, 86, 62), List(54, 86, 174, 134), List(42, 62, 134, 106)))
  }
}
