case class Point(var x: Double, var y: Double, var z: Double)

case class Edge(var pn1: Int, var pn2: Int, var fn1: Int, var fn2: Int, var cp: Point)

case class PointEx(var p: Point, var n: Int)

object catmull_clark_subdivision_surface {
  def indexOf(s: String, ch: String): Int = {
    var i = 0
    while (i < s.length) {
      if (s.substring(i, i + 1) == ch) {
        return i
      }
      i += 1
    }
    return -1
  }
  
  def fmt4(x: Double): String = {
    var y = x * 10000
    if (y >= 0) {
      y += 0.5
    } else {
      y -= 0.5
    }
    y = (y.toInt).toDouble / 10000
    var s = y.toString
    var dot = indexOf(s, ".")
    if (dot == 0 - 1) {
      s += ".0000"
    } else {
      var decs = s.length - dot - 1
      if (decs > 4) {
        s = s.substring(0, dot + 5)
      } else {
        while (decs < 4) {
          s += "0"
          decs += 1
        }
      }
    }
    if (x >= 0) {
      s = " " + s
    }
    return s
  }
  
  def fmt2(n: Int): String = {
    val s = n.toString
    if (s.length < 2) {
      return " " + s
    }
    return s
  }
  
  def sumPoint(p1: Point, p2: Point): Point = Point(x = p1.x + p2.x, y = p1.y + p2.y, z = p1.z + p2.z)
  
  def mulPoint(p: Point, m: Double): Point = Point(x = p.x * m, y = p.y * m, z = p.z * m)
  
  def divPoint(p: Point, d: Double): Point = mulPoint(p, 1 / d)
  
  def centerPoint(p1: Point, p2: Point): Point = divPoint(sumPoint(p1, p2), 2)
  
  def getFacePoints(points: List[Point], faces: List[List[Int]]): List[Point] = {
    var facePoints: List[Point] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < faces.length) {
      val face = (faces).apply(i)
      var fp = Point(x = 0, y = 0, z = 0)
      for(idx <- face) {
        fp = sumPoint(fp, (points).apply(idx))
      }
      fp = divPoint(fp, (face.length.toDouble))
      facePoints = facePoints :+ fp
      i += 1
    }
    return facePoints
  }
  
  def sortEdges(edges: List[List[Int]]): List[List[Int]] = {
    var res: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    var tmp = edges
    while (tmp.length > 0) {
      var min = (tmp).apply(0)
      var idx = 0
      var j = 1
      while (j < tmp.length) {
        val e = (tmp).apply(j)
        if ((e).apply(0) < (min).apply(0) || ((e).apply(0) == (min).apply(0) && ((e).apply(1) < (min).apply(1) || ((e).apply(1) == (min).apply(1) && (e).apply(2) < (min).apply(2))))) {
          min = e
          idx = j
        }
        j += 1
      }
      res = res :+ min
      var out: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
      var k = 0
      while (k < tmp.length) {
        if (k != idx) {
          out = out :+ (tmp).apply(k)
        }
        k += 1
      }
      tmp = out
    }
    return res
  }
  
  def getEdgesFaces(points: List[Point], faces: List[List[Int]]): List[Edge] = {
    var edges: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    var fnum = 0
    while (fnum < faces.length) {
      val face = (faces).apply(fnum)
      var numP = face.length
      var pi = 0
      while (pi < numP) {
        var pn1 = (face).apply(pi)
        var pn2 = 0
        if (pi < numP - 1) {
          pn2 = (face).apply(pi + 1)
        } else {
          pn2 = (face).apply(0)
        }
        if (pn1 > pn2) {
          var tmpn = pn1
          pn1 = pn2
          pn2 = tmpn
        }
        edges = edges :+ List(pn1, pn2, fnum)
        pi += 1
      }
      fnum += 1
    }
    edges = sortEdges(edges)
    var merged: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    var idx = 0
    while (idx < edges.length) {
      val e1 = (edges).apply(idx)
      if (idx < edges.length - 1) {
        val e2 = (edges).apply(idx + 1)
        if ((e1).apply(0) == (e2).apply(0) && (e1).apply(1) == (e2).apply(1)) {
          merged = merged :+ List((e1).apply(0), (e1).apply(1), (e1).apply(2), (e2).apply(2))
          idx += 2
          // continue
        }
      }
      merged = merged :+ List((e1).apply(0), (e1).apply(1), (e1).apply(2), -1)
      idx += 1
    }
    var edgesCenters: List[Edge] = scala.collection.mutable.ArrayBuffer[Any]()
    for(me <- merged) {
      val p1 = (points).apply((me).apply(0))
      val p2 = (points).apply((me).apply(1))
      val cp = centerPoint(p1, p2)
      edgesCenters = edgesCenters :+ Edge(pn1 = (me).apply(0), pn2 = (me).apply(1), fn1 = (me).apply(2), fn2 = (me).apply(3), cp = cp)
    }
    return edgesCenters
  }
  
  def getEdgePoints(points: List[Point], edgesFaces: List[Edge], facePoints: List[Point]): List[Point] = {
    var edgePoints: List[Point] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < edgesFaces.length) {
      val edge = (edgesFaces).apply(i)
      val cp = edge.cp
      val fp1 = (facePoints).apply(edge.fn1)
      var fp2 = fp1
      if (edge.fn2 != 0 - 1) {
        fp2 = (facePoints).apply(edge.fn2)
      }
      val cfp = centerPoint(fp1, fp2)
      edgePoints = edgePoints :+ centerPoint(cp, cfp)
      i += 1
    }
    return edgePoints
  }
  
  def getAvgFacePoints(points: List[Point], faces: List[List[Int]], facePoints: List[Point]): List[Point] = {
    var numP = points.length
    var temp: List[PointEx] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < numP) {
      temp = temp :+ PointEx(p = Point(x = 0, y = 0, z = 0), n = 0)
      i += 1
    }
    var fnum = 0
    while (fnum < faces.length) {
      val fp = (facePoints).apply(fnum)
      for(pn <- (faces).apply(fnum)) {
        val tp = (temp).apply(pn)
        temp(pn) = PointEx(p = sumPoint(tp.p, fp), n = tp.n + 1)
      }
      fnum += 1
    }
    var avg: List[Point] = scala.collection.mutable.ArrayBuffer[Any]()
    var j = 0
    while (j < numP) {
      val tp = (temp).apply(j)
      avg = avg :+ divPoint(tp.p, tp.n.toDouble)
      j += 1
    }
    return avg
  }
  
  def getAvgMidEdges(points: List[Point], edgesFaces: List[Edge]): List[Point] = {
    var numP = points.length
    var temp: List[PointEx] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < numP) {
      temp = temp :+ PointEx(p = Point(x = 0, y = 0, z = 0), n = 0)
      i += 1
    }
    for(edge <- edgesFaces) {
      val cp = edge.cp
      var arr = scala.collection.mutable.ArrayBuffer(edge.pn1, edge.pn2)
      for(pn <- arr) {
        val tp = (temp).apply(pn)
        temp(pn) = PointEx(p = sumPoint(tp.p, cp), n = tp.n + 1)
      }
    }
    var avg: List[Point] = scala.collection.mutable.ArrayBuffer[Any]()
    var j = 0
    while (j < numP) {
      val tp = (temp).apply(j)
      avg = avg :+ divPoint(tp.p, tp.n.toDouble)
      j += 1
    }
    return avg
  }
  
  def getPointsFaces(points: List[Point], faces: List[List[Int]]): List[Int] = {
    var pf: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < points.length) {
      pf = pf :+ 0
      i += 1
    }
    var fnum = 0
    while (fnum < faces.length) {
      for(pn <- (faces).apply(fnum)) {
        pf(pn) = (pf).apply(pn) + 1
      }
      fnum += 1
    }
    return pf
  }
  
  def getNewPoints(points: List[Point], pf: List[Int], afp: List[Point], ame: List[Point]): List[Point] = {
    var newPts: List[Point] = scala.collection.mutable.ArrayBuffer[Any]()
    var i = 0
    while (i < points.length) {
      var n = (pf).apply(i).toDouble
      var m1 = (n - 3) / n
      var m2 = 1 / n
      var m3 = 2 / n
      val old = (points).apply(i)
      val p1 = mulPoint(old, m1)
      val p2 = mulPoint((afp).apply(i), m2)
      val p3 = mulPoint((ame).apply(i), m3)
      newPts = newPts :+ sumPoint(sumPoint(p1, p2), p3)
      i += 1
    }
    return newPts
  }
  
  def key(a: Int, b: Int): String = {
    if (a < b) {
      return a.toString + "," + b.toString
    }
    return b.toString + "," + a.toString
  }
  
  def cmcSubdiv(points: List[Point], faces: List[List[Int]]): List[any] = {
    val facePoints = getFacePoints(points, faces)
    val edgesFaces = getEdgesFaces(points, faces)
    val edgePoints = getEdgePoints(points, edgesFaces, facePoints)
    val avgFacePoints = getAvgFacePoints(points, faces, facePoints)
    val avgMidEdges = getAvgMidEdges(points, edgesFaces)
    val pointsFaces = getPointsFaces(points, faces)
    var newPoints = getNewPoints(points, pointsFaces, avgFacePoints, avgMidEdges)
    var facePointNums: List[Int] = scala.collection.mutable.ArrayBuffer[Any]()
    var nextPoint = newPoints.length
    for(fp <- facePoints) {
      newPoints = newPoints :+ fp
      facePointNums = facePointNums :+ nextPoint
      nextPoint += 1
    }
    var edgePointNums: Map[String, Int] = scala.collection.mutable.Map()
    var idx = 0
    while (idx < edgesFaces.length) {
      val e = (edgesFaces).apply(idx)
      newPoints = newPoints :+ (edgePoints).apply(idx)
      edgePointNums(key(e.pn1, e.pn2)) = nextPoint
      nextPoint += 1
      idx += 1
    }
    var newFaces: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    var fnum = 0
    while (fnum < faces.length) {
      val oldFace = (faces).apply(fnum)
      if (oldFace.length == 4) {
        val a = (oldFace).apply(0)
        val b = (oldFace).apply(1)
        val c = (oldFace).apply(2)
        val d = (oldFace).apply(3)
        val fpnum = (facePointNums).apply(fnum)
        val ab = (edgePointNums).apply(key(a, b))
        val da = (edgePointNums).apply(key(d, a))
        val bc = (edgePointNums).apply(key(b, c))
        val cd = (edgePointNums).apply(key(c, d))
        newFaces = newFaces :+ List(a, ab, fpnum, da)
        newFaces = newFaces :+ List(b, bc, fpnum, ab)
        newFaces = newFaces :+ List(c, cd, fpnum, bc)
        newFaces = newFaces :+ List(d, da, fpnum, cd)
      }
      fnum += 1
    }
    return List(newPoints, newFaces)
  }
  
  def formatPoint(p: Point): String = "[" + fmt4(p.x) + " " + fmt4(p.y) + " " + fmt4(p.z) + "]"
  
  def formatFace(f: List[Int]): String = {
    if (f.length == 0) {
      return "[]"
    }
    var s = "[" + fmt2((f).apply(0))
    var i = 1
    while (i < f.length) {
      s = s + " " + fmt2((f).apply(i))
      i += 1
    }
    s += "]"
    return s
  }
  
  def main() = {
    val inputPoints = List(Point(x = -1, y = 1, z = 1), Point(x = -1, y = -1, z = 1), Point(x = 1, y = -1, z = 1), Point(x = 1, y = 1, z = 1), Point(x = 1, y = -1, z = -1), Point(x = 1, y = 1, z = -1), Point(x = -1, y = -1, z = -1), Point(x = -1, y = 1, z = -1))
    val inputFaces = List(List(0, 1, 2, 3), List(3, 2, 4, 5), List(5, 4, 6, 7), List(7, 0, 3, 5), List(7, 6, 1, 0), List(6, 1, 2, 4))
    var outputPoints = inputPoints
    var outputFaces = inputFaces
    var i = 0
    while (i < 1) {
      val res = cmcSubdiv(outputPoints, outputFaces)
      outputPoints = (res).apply(0)
      outputFaces = (res).apply(1)
      i += 1
    }
    for(p <- outputPoints) {
      println(formatPoint(p))
    }
    println("")
    for(f <- outputFaces) {
      println(formatFace(f))
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
