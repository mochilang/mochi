case class Point(var x: Double, var y: Double)

object circles_of_given_radius_through_two_points {
  def sqrtApprox(x: Double): Double = {
    var g = x
    var i = 0
    while (i < 40) {
      g = (g + x / g) / 2
      i += 1
    }
    return g
  }
  
  def hypot(x: Double, y: Double): Double = sqrtApprox(x * x + y * y)
  
  def circles(p1: Point, p2: Point, r: Double): List[any] = {
    if (p1.x == p2.x && p1.y == p2.y) {
      if (r == 0) {
        return List(p1, p1, "Coincident points with r==0.0 describe a degenerate circle.")
      }
      return List(p1, p2, "Coincident points describe an infinite number of circles.")
    }
    if (r == 0) {
      return List(p1, p2, "R==0.0 does not describe circles.")
    }
    val dx = p2.x - p1.x
    val dy = p2.y - p1.y
    val q = hypot(dx, dy)
    if (q > 2 * r) {
      return List(p1, p2, "Points too far apart to form circles.")
    }
    val m = Point(x = (p1.x + p2.x) / 2, y = (p1.y + p2.y) / 2)
    if (q == 2 * r) {
      return List(m, m, "Points form a diameter and describe only a single circle.")
    }
    val d = sqrtApprox(r * r - q * q / 4)
    val ox = d * dx / q
    val oy = d * dy / q
    return List(Point(x = m.x - oy, y = m.y + ox), Point(x = m.x + oy, y = m.y - ox), "Two circles.")
  }
  
  def main(args: Array[String]): Unit = {
    val Two = "Two circles."
    val R0 = "R==0.0 does not describe circles."
    val Co = "Coincident points describe an infinite number of circles."
    val CoR0 = "Coincident points with r==0.0 describe a degenerate circle."
    val Diam = "Points form a diameter and describe only a single circle."
    val Far = "Points too far apart to form circles."
    var td = scala.collection.mutable.ArrayBuffer(List(Point(x = 0.1234, y = 0.9876), Point(x = 0.8765, y = 0.2345), 2), List(Point(x = 0, y = 2), Point(x = 0, y = 0), 1), List(Point(x = 0.1234, y = 0.9876), Point(x = 0.1234, y = 0.9876), 2), List(Point(x = 0.1234, y = 0.9876), Point(x = 0.8765, y = 0.2345), 0.5), List(Point(x = 0.1234, y = 0.9876), Point(x = 0.1234, y = 0.9876), 0))
    for(tc <- td) {
      val p1 = (tc).apply(0)
      val p2 = (tc).apply(1)
      val r = (tc).apply(2)
      println(((("p1:  {" + p1.x.toString).asInstanceOf[Int] + " ").asInstanceOf[Int] + p1.y.toString).asInstanceOf[Int] + "}")
      println(((("p2:  {" + p2.x.toString).asInstanceOf[Int] + " ").asInstanceOf[Int] + p2.y.toString).asInstanceOf[Int] + "}")
      println("r:  " + r.toString)
      val res = circles(p1, p2, r)
      val c1 = (res).apply(0)
      val c2 = (res).apply(1)
      val caseStr = (res).apply(2)
      println("   " + (caseStr).asInstanceOf[Int])
      if ((caseStr).asInstanceOf[Int] == "Points form a diameter and describe only a single circle." || (caseStr).asInstanceOf[Int] == "Coincident points with r==0.0 describe a degenerate circle.") {
        println(((("   Center:  {" + c1.x.toString).asInstanceOf[Int] + " ").asInstanceOf[Int] + c1.y.toString).asInstanceOf[Int] + "}")
      } else {
        if ((caseStr).asInstanceOf[Int] == "Two circles.") {
          println(((("   Center 1:  {" + c1.x.toString).asInstanceOf[Int] + " ").asInstanceOf[Int] + c1.y.toString).asInstanceOf[Int] + "}")
          println(((("   Center 2:  {" + c2.x.toString).asInstanceOf[Int] + " ").asInstanceOf[Int] + c2.y.toString).asInstanceOf[Int] + "}")
        }
      }
      println("")
    }
  }
}
