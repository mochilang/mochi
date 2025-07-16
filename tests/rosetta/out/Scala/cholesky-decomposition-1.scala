object cholesky_decomposition_1 {
  def sqrtApprox(x: Double): Double = {
    var guess = x
    var i = 0
    while (i < 20) {
      guess = (guess + x / guess) / 2
      i += 1
    }
    return guess
  }
  
  def makeSym(order: Int, elements: List[Double]): Map[String, any] = Map("order" -> order, "ele" -> elements)
  
  def unpackSym(m: Map[String, any]): List[List[Double]] = {
    val n = (m).apply("order")
    val ele = (m).apply("ele")
    var mat: List[List[Double]] = scala.collection.mutable.ArrayBuffer[Any]()
    var idx = 0
    var r = 0
    while (r < (n).asInstanceOf[Int]) {
      var row: List[Double] = scala.collection.mutable.ArrayBuffer[Any]()
      var c = 0
      while (c <= r) {
        row = row :+ (ele).apply(idx)
        idx += 1
        c += 1
      }
      while (c < (n).asInstanceOf[Int]) {
        row = row :+ 0
        c += 1
      }
      mat = mat :+ row
      r += 1
    }
    r = 0
    while (r < (n).asInstanceOf[Int]) {
      var c = r + 1
      while (c < (n).asInstanceOf[Int]) {
        val _tmp0 = mat(r).updated(c, ((mat).apply(c)).apply(r))
        mat = mat.updated(r, _tmp0)
        c += 1
      }
      r += 1
    }
    return mat
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
  
  def printSym(m: Map[String, any]) = {
    printMat(unpackSym(m))
  }
  
  def printLower(m: Map[String, any]) = {
    val n = (m).apply("order")
    val ele = (m).apply("ele")
    var mat: List[List[Double]] = scala.collection.mutable.ArrayBuffer[Any]()
    var idx = 0
    var r = 0
    while (r < (n).asInstanceOf[Int]) {
      var row: List[Double] = scala.collection.mutable.ArrayBuffer[Any]()
      var c = 0
      while (c <= r) {
        row = row :+ (ele).apply(idx)
        idx += 1
        c += 1
      }
      while (c < (n).asInstanceOf[Int]) {
        row = row :+ 0
        c += 1
      }
      mat = mat :+ row
      r += 1
    }
    printMat(mat)
  }
  
  def choleskyLower(a: Map[String, any]): Map[String, any] = {
    val n = (a).apply("order")
    val ae = (a).apply("ele")
    var le: List[Double] = scala.collection.mutable.ArrayBuffer[Any]()
    var idx = 0
    while (idx < ae.length) {
      le = le :+ 0
      idx += 1
    }
    var row = 1
    var col = 1
    var dr = 0
    var dc = 0
    var i = 0
    while (i < ae.length) {
      val e = (ae).apply(i)
      if (i < dr) {
        var d = (((e).asInstanceOf[Int] - (le).apply(i))).asInstanceOf[Int] / (le).apply(dc)
        le(i) = d
        var ci = col
        var cx = dc
        var j = i + 1
        while (j <= dr) {
          cx += ci
          ci += 1
          le(j) = (le).apply(j) + (d).asInstanceOf[Int] * (le).apply(cx)
          j += 1
        }
        col += 1
        dc += col
      } else {
        le(i) = sqrtApprox((e).asInstanceOf[Int] - (le).apply(i))
        row += 1
        dr += row
        col = 1
        dc = 0
      }
      i += 1
    }
    return Map("order" -> n, "ele" -> le)
  }
  
  def demo(a: Map[String, any]) = {
    println("A:")
    printSym(a)
    println("L:")
    val l = choleskyLower(a)
    printLower(l)
  }
  
  def main(args: Array[String]): Unit = {
    demo(makeSym(3, List(25, 15, 18, -5, 0, 11)))
    demo(makeSym(4, List(18, 22, 70, 54, 86, 174, 42, 62, 134, 106)))
  }
}
