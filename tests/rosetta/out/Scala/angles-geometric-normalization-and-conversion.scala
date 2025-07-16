object angles_geometric_normalization_and_conversion {
  def d2d(d: Double): Double = d % 360
  
  def g2g(g: Double): Double = g % 400
  
  def m2m(m: Double): Double = m % 6400
  
  def r2r(r: Double): Double = r % (2 * 3.141592653589793)
  
  def d2g(d: Double): Double = d2d(d) * 400 / 360
  
  def d2m(d: Double): Double = d2d(d) * 6400 / 360
  
  def d2r(d: Double): Double = d2d(d) * 3.141592653589793 / 180
  
  def g2d(g: Double): Double = g2g(g) * 360 / 400
  
  def g2m(g: Double): Double = g2g(g) * 6400 / 400
  
  def g2r(g: Double): Double = g2g(g) * 3.141592653589793 / 200
  
  def m2d(m: Double): Double = m2m(m) * 360 / 6400
  
  def m2g(m: Double): Double = m2m(m) * 400 / 6400
  
  def m2r(m: Double): Double = m2m(m) * 3.141592653589793 / 3200
  
  def r2d(r: Double): Double = r2r(r) * 180 / 3.141592653589793
  
  def r2g(r: Double): Double = r2r(r) * 200 / 3.141592653589793
  
  def r2m(r: Double): Double = r2r(r) * 3200 / 3.141592653589793
  
  def main() = {
    val angles = List(-2, -1, 0, 1, 2, 6.2831853, 16, 57.2957795, 359, 399, 6399, 1e+06)
    println("degrees normalized_degs gradians mils radians")
    for(a <- angles) {
      println(a.toString + " " + d2d(a).toString + " " + d2g(a).toString + " " + d2m(a).toString + " " + d2r(a).toString)
    }
    println("\ngradians normalized_grds degrees mils radians")
    for(a <- angles) {
      println(a.toString + " " + g2g(a).toString + " " + g2d(a).toString + " " + g2m(a).toString + " " + g2r(a).toString)
    }
    println("\nmils normalized_mils degrees gradians radians")
    for(a <- angles) {
      println(a.toString + " " + m2m(a).toString + " " + m2d(a).toString + " " + m2g(a).toString + " " + m2r(a).toString)
    }
    println("\nradians normalized_rads degrees gradians mils")
    for(a <- angles) {
      println(a.toString + " " + r2r(a).toString + " " + r2d(a).toString + " " + r2g(a).toString + " " + r2m(a).toString)
    }
  }
  
  def main(args: Array[String]): Unit = {
    main()
  }
}
