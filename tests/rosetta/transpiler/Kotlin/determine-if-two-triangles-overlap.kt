import java.math.BigInteger

data class Point(var x: Double = 0.0, var y: Double = 0.0)
data class Triangle(var p1: Point = Point(x = 0.0, y = 0.0), var p2: Point = Point(x = 0.0, y = 0.0), var p3: Point = Point(x = 0.0, y = 0.0))
fun fmt1(f: Double): String {
    var s: String = f.toString()
    var idx: Int = s.indexOf(".")
    if (idx < 0) {
        s = s + ".0"
    } else {
        var need: BigInteger = (idx + 2).toBigInteger()
        if ((s.length).toBigInteger().compareTo((need)) > 0) {
            s = s.substring(0, (need).toInt())
        } else {
            while ((s.length).toBigInteger().compareTo((need)) < 0) {
                s = s + "0"
            }
        }
    }
    return s
}

fun pointStr(p: Point): String {
    return ((("(" + fmt1(p.x)) + ", ") + fmt1(p.y)) + ")"
}

fun triangleStr(t: Triangle): String {
    return (((("Triangle " + pointStr(t.p1)) + ", ") + pointStr(t.p2)) + ", ") + pointStr(t.p3)
}

fun orient(a: Point, b: Point, c: Point): Double {
    return ((b.x - a.x) * (c.y - a.y)) - ((b.y - a.y) * (c.x - a.x))
}

fun pointInTri(p: Point, t: Triangle, onBoundary: Boolean): Boolean {
    var d1: Double = orient(p, t.p1, t.p2)
    var d2: Double = orient(p, t.p2, t.p3)
    var d3: Double = orient(p, t.p3, t.p1)
    var hasNeg: Boolean = (((d1 < 0.0) || (d2 < 0.0) as Boolean)) || (d3 < 0.0)
    var hasPos: Boolean = (((d1 > 0.0) || (d2 > 0.0) as Boolean)) || (d3 > 0.0)
    if ((onBoundary as Boolean)) {
        return (!((hasNeg && hasPos) as Boolean) as Boolean)
    }
    return (((((((!((hasNeg && hasPos) as Boolean) as Boolean) && (d1 != 0.0) as Boolean)) && (d2 != 0.0) as Boolean)) && (d3 != 0.0)) as Boolean)
}

fun edgeCheck(a0: Point, a1: Point, bs: MutableList<Point>, onBoundary: Boolean): Boolean {
    var d0: Double = orient(a0, a1, bs[0]!!)
    var d1: Double = orient(a0, a1, bs[1]!!)
    var d2: Double = orient(a0, a1, bs[2]!!)
    if ((onBoundary as Boolean)) {
        return (((((d0 <= 0.0) && (d1 <= 0.0) as Boolean)) && (d2 <= 0.0)) as Boolean)
    }
    return (((((d0 < 0.0) && (d1 < 0.0) as Boolean)) && (d2 < 0.0)) as Boolean)
}

fun triTri2D(t1: Triangle, t2: Triangle, onBoundary: Boolean): Boolean {
    var a: MutableList<Point> = mutableListOf(t1.p1, t1.p2, t1.p3)
    var b: MutableList<Point> = mutableListOf(t2.p1, t2.p2, t2.p3)
    var i: Int = 0
    while (i < 3) {
        var j: BigInteger = (Math.floorMod((i + 1), 3)).toBigInteger()
        if (((edgeCheck(a[i]!!, a[(j).toInt()]!!, b, onBoundary)) as Boolean)) {
            return false
        }
        i = i + 1
    }
    i = 0
    while (i < 3) {
        var j: BigInteger = (Math.floorMod((i + 1), 3)).toBigInteger()
        if (((edgeCheck(b[i]!!, b[(j).toInt()]!!, a, onBoundary)) as Boolean)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun iff(cond: Boolean, a: String, b: String): String {
    if ((cond as Boolean)) {
        return a
    } else {
        return b
    }
}

fun user_main(): Unit {
    var t1: Triangle = Triangle(p1 = Point(x = 0.0, y = 0.0), p2 = Point(x = 5.0, y = 0.0), p3 = Point(x = 0.0, y = 5.0))
    var t2: Triangle = Triangle(p1 = Point(x = 0.0, y = 0.0), p2 = Point(x = 5.0, y = 0.0), p3 = Point(x = 0.0, y = 6.0))
    println(triangleStr(t1) + " and")
    println(triangleStr(t2))
    var overlapping: Boolean = triTri2D(t1, t2, true)
    println(iff(overlapping, "overlap", "do not overlap"))
    println("")
    t1 = Triangle(p1 = Point(x = 0.0, y = 0.0), p2 = Point(x = 0.0, y = 5.0), p3 = Point(x = 5.0, y = 0.0))
    t2 = t1
    println(triangleStr(t1) + " and")
    println(triangleStr(t2))
    overlapping = triTri2D(t1, t2, true)
    println(iff(overlapping, "overlap (reversed)", "do not overlap"))
    println("")
    t1 = Triangle(p1 = Point(x = 0.0, y = 0.0), p2 = Point(x = 5.0, y = 0.0), p3 = Point(x = 0.0, y = 5.0))
    t2 = Triangle(p1 = Point(x = 0.0 - 10.0, y = 0.0), p2 = Point(x = 0.0 - 5.0, y = 0.0), p3 = Point(x = 0.0 - 1.0, y = 6.0))
    println(triangleStr(t1) + " and")
    println(triangleStr(t2))
    overlapping = triTri2D(t1, t2, true)
    println(iff(overlapping, "overlap", "do not overlap"))
    println("")
    t1.p3 = Point(x = 2.5, y = 5.0)
    t2 = Triangle(p1 = Point(x = 0.0, y = 4.0), p2 = Point(x = 2.5, y = 0.0 - 1.0), p3 = Point(x = 5.0, y = 4.0))
    println(triangleStr(t1) + " and")
    println(triangleStr(t2))
    overlapping = triTri2D(t1, t2, true)
    println(iff(overlapping, "overlap", "do not overlap"))
    println("")
    t1 = Triangle(p1 = Point(x = 0.0, y = 0.0), p2 = Point(x = 1.0, y = 1.0), p3 = Point(x = 0.0, y = 2.0))
    t2 = Triangle(p1 = Point(x = 2.0, y = 1.0), p2 = Point(x = 3.0, y = 0.0), p3 = Point(x = 3.0, y = 2.0))
    println(triangleStr(t1) + " and")
    println(triangleStr(t2))
    overlapping = triTri2D(t1, t2, true)
    println(iff(overlapping, "overlap", "do not overlap"))
    println("")
    t2 = Triangle(p1 = Point(x = 2.0, y = 1.0), p2 = Point(x = 3.0, y = 0.0 - 2.0), p3 = Point(x = 3.0, y = 4.0))
    println(triangleStr(t1) + " and")
    println(triangleStr(t2))
    overlapping = triTri2D(t1, t2, true)
    println(iff(overlapping, "overlap", "do not overlap"))
    println("")
    t1 = Triangle(p1 = Point(x = 0.0, y = 0.0), p2 = Point(x = 1.0, y = 0.0), p3 = Point(x = 0.0, y = 1.0))
    t2 = Triangle(p1 = Point(x = 1.0, y = 0.0), p2 = Point(x = 2.0, y = 0.0), p3 = Point(x = 1.0, y = 1.1))
    println(triangleStr(t1) + " and")
    println(triangleStr(t2))
    println("which have only a single corner in contact, if boundary points collide")
    overlapping = triTri2D(t1, t2, true)
    println(iff(overlapping, "overlap", "do not overlap"))
    println("")
    println(triangleStr(t1) + " and")
    println(triangleStr(t2))
    println("which have only a single corner in contact, if boundary points do not collide")
    overlapping = triTri2D(t1, t2, false)
    println(iff(overlapping, "overlap", "do not overlap"))
}

fun main() {
    user_main()
}
