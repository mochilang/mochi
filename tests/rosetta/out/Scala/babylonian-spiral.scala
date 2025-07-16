object babylonian_spiral {
  def push(h: List[Map[String, Int]], it: Map[String, Int]): List[Map[String, Int]] = {
    h = h :+ it
    var i = h.length - 1
    while (i > 0 && ((h).apply(i - 1)).apply("s") > ((h).apply(i)).apply("s")) {
      val tmp = (h).apply(i - 1)
      h(i - 1) = (h).apply(i)
      h(i) = tmp
      i -= 1
    }
    return h
  }
  
  def step(h: List[Map[String, Int]], nv: Int, dir: List[Int]): Map[String, any] = {
    while ((h.length == 0 || nv * nv).asInstanceOf[Int] <= ((h).apply(0)).apply("s")) {
      h = push(h, Map("s" -> nv * nv, "a" -> nv, "b" -> 0))
      nv += 1
    }
    val s = ((h).apply(0)).apply("s")
    var v: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    while (h.length > 0 && ((h).apply(0)).apply("s") == s) {
      val it = (h).apply(0)
      h = h.slice(1, h.length)
      v = v :+ List((it).apply("a"), (it).apply("b"))
      if ((it).apply("a") > (it).apply("b")) {
        h = push(h, Map("s" -> (it).apply("a") * (it).apply("a") + ((it).apply("b") + 1) * ((it).apply("b") + 1), "a" -> (it).apply("a"), "b" -> (it).apply("b") + 1))
      }
    }
    var list: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    for(p <- v) {
      list = list :+ p
    }
    var temp: List[List[Int]] = list
    for(p <- temp) {
      if ((p).apply(0) != (p).apply(1)) {
        list = list :+ List((p).apply(1), (p).apply(0))
      }
    }
    temp = list
    for(p <- temp) {
      if ((p).apply(1) != 0) {
        list = list :+ List((p).apply(0), -(p).apply(1))
      }
    }
    temp = list
    for(p <- temp) {
      if ((p).apply(0) != 0) {
        list = list :+ List(-(p).apply(0), (p).apply(1))
      }
    }
    var bestDot = -999999999
    var best = dir
    for(p <- list) {
      val cross = (p).apply(0) * (dir).apply(1) - (p).apply(1) * (dir).apply(0)
      if (cross >= 0) {
        val dot = (p).apply(0) * (dir).apply(0) + (p).apply(1) * (dir).apply(1)
        if (dot > bestDot) {
          bestDot = dot
          best = p
        }
      }
    }
    return Map("d" -> best, "heap" -> h, "n" -> nv)
  }
  
  def positions(n: Int): List[List[Int]] = {
    var pos: List[List[Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    var x = 0
    var y = 0
    var dir: List[Int] = scala.collection.mutable.ArrayBuffer(0, 1)
    var heap: List[Map[String, Int]] = scala.collection.mutable.ArrayBuffer[Any]()
    var nv = 1
    var i = 0
    while (i < n) {
      pos = pos :+ List(x, y)
      val st = step(heap, nv, dir)
      dir = (st).apply("d").asInstanceOf[List[Int]]
      heap = (st).apply("heap").asInstanceOf[List[Map[String, Int]]]
      nv = (st).apply("n").toInt
      x += (dir).apply(0)
      y += (dir).apply(1)
      i += 1
    }
    return pos
  }
  
  def pad(s: String, w: Int): String = {
    var r = s
    while (r.length < w) {
      r += " "
    }
    return r
  }
  
  def main() = {
    val pts = positions(40)
    println("The first 40 Babylonian spiral points are:")
    var line = ""
    var i = 0
    while (i < pts.length) {
      val p = (pts).apply(i)
      val s = pad("(" + (p).apply(0).toString + ", " + (p).apply(1).toString + ")", 10)
      line += s
      if ((i + 1) % 10 == 0) {
        println(line)
        line = ""
      }
      i += 1
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
