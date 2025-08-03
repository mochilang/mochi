import java.math.BigInteger

data class Point(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)
data class Edge(var pn1: Int = 0, var pn2: Int = 0, var fn1: Int = 0, var fn2: Int = 0, var cp: Point = Point(x = 0.0, y = 0.0, z = 0.0))
data class PointEx(var p: Point = Point(x = 0.0, y = 0.0, z = 0.0), var n: Int = 0)
fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun fmt4(x: Double): String {
    var y: Double = x * 10000.0
    if (y >= 0) {
        y = y + 0.5
    } else {
        y = y - 0.5
    }
    y = ((((y.toInt())).toDouble())) / 10000.0
    var s: String = y.toString()
    var dot: Int = s.indexOf(".")
    if (dot == (0 - 1)) {
        s = s + ".0000"
    } else {
        var decs: BigInteger = ((s.length - dot) - 1).toBigInteger()
        if (decs.compareTo((4).toBigInteger()) > 0) {
            s = s.substring(0, dot + 5)
        } else {
            while (decs.compareTo((4).toBigInteger()) < 0) {
                s = s + "0"
                decs = decs.add((1).toBigInteger())
            }
        }
    }
    if (x >= 0.0) {
        s = " " + s
    }
    return s
}

fun fmt2(n: Int): String {
    var s: String = n.toString()
    if (s.length < 2) {
        return " " + s
    }
    return s
}

fun sumPoint(p1: Point, p2: Point): Point {
    return Point(x = p1.x + p2.x, y = p1.y + p2.y, z = p1.z + p2.z)
}

fun mulPoint(p: Point, m: Double): Point {
    return Point(x = p.x * m, y = p.y * m, z = p.z * m)
}

fun divPoint(p: Point, d: Double): Point {
    return mulPoint(p, 1.0 / d)
}

fun centerPoint(p1: Point, p2: Point): Point {
    return divPoint(sumPoint(p1, p2), 2.0)
}

fun getFacePoints(points: MutableList<Point>, faces: MutableList<MutableList<Int>>): MutableList<Point> {
    var facePoints: MutableList<Point> = mutableListOf<Point>()
    var i: Int = 0
    while (i < faces.size) {
        var face: MutableList<Int> = faces[i]!!
        var fp: Point = Point(x = 0.0, y = 0.0, z = 0.0)
        for (idx in face) {
            fp = sumPoint(fp, points[idx]!!)
        }
        fp = divPoint(fp, (face.size.toDouble()))
        facePoints = run { val _tmp = facePoints.toMutableList(); _tmp.add(fp); _tmp }
        i = i + 1
    }
    return facePoints
}

fun sortEdges(edges: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var tmp: MutableList<MutableList<Int>> = edges
    while (tmp.size > 0) {
        var min: MutableList<Int> = tmp[0]!!
        var idx: Int = 0
        var j: Int = 1
        while (j < tmp.size) {
            var e: MutableList<Int> = tmp[j]!!
            if ((e[0]!! < min[0]!!) || (((e[0]!! == min[0]!!) && (((e[1]!! < min[1]!!) || (((e[1]!! == min[1]!!) && (e[2]!! < min[2]!!) as Boolean)) as Boolean)) as Boolean))) {
                min = e
                idx = j
            }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(min); _tmp }
        var out: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
        var k: Int = 0
        while (k < tmp.size) {
            if (k != idx) {
                out = run { val _tmp = out.toMutableList(); _tmp.add(tmp[k]!!); _tmp }
            }
            k = k + 1
        }
        tmp = out
    }
    return res
}

fun getEdgesFaces(points: MutableList<Point>, faces: MutableList<MutableList<Int>>): MutableList<Edge> {
    var edges: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var fnum: Int = 0
    while (fnum < faces.size) {
        var face: MutableList<Int> = faces[fnum]!!
        var numP: Int = face.size
        var pi: Int = 0
        while (pi < numP) {
            var pn1: Int = face[pi]!!
            var pn2: Int = 0
            if (pi < (numP - 1)) {
                pn2 = face[pi + 1]!!
            } else {
                pn2 = face[0]!!
            }
            if (pn1 > pn2) {
                var tmpn: Int = pn1
                pn1 = pn2
                pn2 = tmpn
            }
            edges = run { val _tmp = edges.toMutableList(); _tmp.add(mutableListOf(pn1, pn2, fnum)); _tmp }
            pi = pi + 1
        }
        fnum = fnum + 1
    }
    edges = sortEdges(edges)
    var merged: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var idx: Int = 0
    while (idx < edges.size) {
        var e1: MutableList<Int> = edges[idx]!!
        if (idx < (edges.size - 1)) {
            var e2: MutableList<Int> = edges[idx + 1]!!
            if ((e1[0]!! == e2[0]!!) && (e1[1]!! == e2[1]!!)) {
                merged = run { val _tmp = merged.toMutableList(); _tmp.add(mutableListOf(e1[0]!!, e1[1]!!, e1[2]!!, e2[2]!!)); _tmp }
                idx = idx + 2
                continue
            }
        }
        merged = run { val _tmp = merged.toMutableList(); _tmp.add(mutableListOf(e1[0]!!, e1[1]!!, e1[2]!!, 0 - 1)); _tmp }
        idx = idx + 1
    }
    var edgesCenters: MutableList<Edge> = mutableListOf<Edge>()
    for (me in merged) {
        var p1: Point = points[me[0]!!]!!
        var p2: Point = points[me[1]!!]!!
        var cp: Point = centerPoint(p1, p2)
        edgesCenters = run { val _tmp = edgesCenters.toMutableList(); _tmp.add(Edge(pn1 = me[0]!!, pn2 = me[1]!!, fn1 = me[2]!!, fn2 = me[3]!!, cp = cp)); _tmp }
    }
    return edgesCenters
}

fun getEdgePoints(points: MutableList<Point>, edgesFaces: MutableList<Edge>, facePoints: MutableList<Point>): MutableList<Point> {
    var edgePoints: MutableList<Point> = mutableListOf<Point>()
    var i: Int = 0
    while (i < edgesFaces.size) {
        var edge: Edge = edgesFaces[i]!!
        var cp: Point = edge.cp
        var fp1: Point = facePoints[edge.fn1]!!
        var fp2: Point = fp1
        if (edge.fn2 != (0 - 1)) {
            fp2 = facePoints[edge.fn2]!!
        }
        var cfp: Point = centerPoint(fp1, fp2)
        edgePoints = run { val _tmp = edgePoints.toMutableList(); _tmp.add(centerPoint(cp, cfp)); _tmp }
        i = i + 1
    }
    return edgePoints
}

fun getAvgFacePoints(points: MutableList<Point>, faces: MutableList<MutableList<Int>>, facePoints: MutableList<Point>): MutableList<Point> {
    var numP: Int = points.size
    var temp: MutableList<PointEx> = mutableListOf<PointEx>()
    var i: Int = 0
    while (i < numP) {
        temp = run { val _tmp = temp.toMutableList(); _tmp.add(PointEx(p = Point(x = 0.0, y = 0.0, z = 0.0), n = 0)); _tmp }
        i = i + 1
    }
    var fnum: Int = 0
    while (fnum < faces.size) {
        var fp: Point = facePoints[fnum]!!
        for (pn in faces[fnum]!!) {
            var tp: PointEx = temp[pn]!!
            temp[pn] = PointEx(p = sumPoint(((tp.p) as Point), fp), n = tp.n + 1)
        }
        fnum = fnum + 1
    }
    var avg: MutableList<Point> = mutableListOf<Point>()
    var j: Int = 0
    while (j < numP) {
        var tp: PointEx = temp[j]!!
        avg = run { val _tmp = avg.toMutableList(); _tmp.add(divPoint(tp.p, ((tp.n).toDouble()))); _tmp }
        j = j + 1
    }
    return avg
}

fun getAvgMidEdges(points: MutableList<Point>, edgesFaces: MutableList<Edge>): MutableList<Point> {
    var numP: Int = points.size
    var temp: MutableList<PointEx> = mutableListOf<PointEx>()
    var i: Int = 0
    while (i < numP) {
        temp = run { val _tmp = temp.toMutableList(); _tmp.add(PointEx(p = Point(x = 0.0, y = 0.0, z = 0.0), n = 0)); _tmp }
        i = i + 1
    }
    for (edge in edgesFaces) {
        var cp: Point = edge.cp
        var arr: MutableList<Int> = mutableListOf(edge.pn1, edge.pn2)
        for (pn in arr) {
            var tp: PointEx = temp[pn]!!
            temp[pn] = PointEx(p = sumPoint(tp.p, cp), n = tp.n + 1)
        }
    }
    var avg: MutableList<Point> = mutableListOf<Point>()
    var j: Int = 0
    while (j < numP) {
        var tp: PointEx = temp[j]!!
        avg = run { val _tmp = avg.toMutableList(); _tmp.add(divPoint(tp.p, ((tp.n).toDouble()))); _tmp }
        j = j + 1
    }
    return avg
}

fun getPointsFaces(points: MutableList<Point>, faces: MutableList<MutableList<Int>>): MutableList<Int> {
    var pf: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < points.size) {
        pf = run { val _tmp = pf.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var fnum: Int = 0
    while (fnum < faces.size) {
        for (pn in faces[fnum]!!) {
            pf[pn] = pf[pn]!! + 1
        }
        fnum = fnum + 1
    }
    return pf
}

fun getNewPoints(points: MutableList<Point>, pf: MutableList<Int>, afp: MutableList<Point>, ame: MutableList<Point>): MutableList<Point> {
    var newPts: MutableList<Point> = mutableListOf<Point>()
    var i: Int = 0
    while (i < points.size) {
        var n: Double = ((pf[i]!!).toDouble())
        var m1: Double = (n - 3.0) / n
        var m2: Double = 1.0 / n
        var m3: Double = 2.0 / n
        var old: Point = points[i]!!
        var p1: Point = mulPoint(old, m1)
        var p2: Point = mulPoint(afp[i]!!, m2)
        var p3: Point = mulPoint(ame[i]!!, m3)
        newPts = run { val _tmp = newPts.toMutableList(); _tmp.add(sumPoint(sumPoint(p1, p2), p3)); _tmp }
        i = i + 1
    }
    return newPts
}

fun key(a: Int, b: Int): String {
    if (a < b) {
        return (a.toString() + ",") + b.toString()
    }
    return (b.toString() + ",") + a.toString()
}

fun cmcSubdiv(points: MutableList<Point>, faces: MutableList<MutableList<Int>>): MutableList<Any?> {
    var facePoints: MutableList<Point> = getFacePoints(points, faces)
    var edgesFaces: MutableList<Edge> = getEdgesFaces(points, faces)
    var edgePoints: MutableList<Point> = getEdgePoints(points, edgesFaces, facePoints)
    var avgFacePoints: MutableList<Point> = getAvgFacePoints(points, faces, facePoints)
    var avgMidEdges: MutableList<Point> = getAvgMidEdges(points, edgesFaces)
    var pointsFaces: MutableList<Int> = getPointsFaces(points, faces)
    var newPoints: MutableList<Point> = getNewPoints(points, pointsFaces, avgFacePoints, avgMidEdges)
    var facePointNums: MutableList<Int> = mutableListOf<Int>()
    var nextPoint: Int = newPoints.size
    for (fp in facePoints) {
        newPoints = run { val _tmp = newPoints.toMutableList(); _tmp.add(fp); _tmp }
        facePointNums = run { val _tmp = facePointNums.toMutableList(); _tmp.add(nextPoint); _tmp }
        nextPoint = nextPoint + 1
    }
    var edgePointNums: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var idx: Int = 0
    while (idx < edgesFaces.size) {
        var e: Edge = edgesFaces[idx]!!
        newPoints = run { val _tmp = newPoints.toMutableList(); _tmp.add(edgePoints[idx]!!); _tmp }
        (edgePointNums)[key(e.pn1, e.pn2)] = nextPoint
        nextPoint = nextPoint + 1
        idx = idx + 1
    }
    var newFaces: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var fnum: Int = 0
    while (fnum < faces.size) {
        var oldFace: MutableList<Int> = faces[fnum]!!
        if (oldFace.size == 4) {
            var a: Int = oldFace[0]!!
            var b: Int = oldFace[1]!!
            var c: Int = oldFace[2]!!
            var d: Int = oldFace[3]!!
            var fpnum: Int = facePointNums[fnum]!!
            var ab: Int = (edgePointNums)[key(a, b)] as Int
            var da: Int = (edgePointNums)[key(d, a)] as Int
            var bc: Int = (edgePointNums)[key(b, c)] as Int
            var cd: Int = (edgePointNums)[key(c, d)] as Int
            newFaces = run { val _tmp = newFaces.toMutableList(); _tmp.add(mutableListOf(a, ab, fpnum, da)); _tmp }
            newFaces = run { val _tmp = newFaces.toMutableList(); _tmp.add(mutableListOf(b, bc, fpnum, ab)); _tmp }
            newFaces = run { val _tmp = newFaces.toMutableList(); _tmp.add(mutableListOf(c, cd, fpnum, bc)); _tmp }
            newFaces = run { val _tmp = newFaces.toMutableList(); _tmp.add(mutableListOf(d, da, fpnum, cd)); _tmp }
        }
        fnum = fnum + 1
    }
    return mutableListOf<Any?>((newPoints as Any?), (newFaces as Any?))
}

fun formatPoint(p: Point): String {
    return ((((("[" + fmt4(p.x)) + " ") + fmt4(p.y)) + " ") + fmt4(p.z)) + "]"
}

fun formatFace(f: MutableList<Int>): String {
    if (f.size == 0) {
        return "[]"
    }
    var s: String = "[" + fmt2(f[0]!!)
    var i: Int = 1
    while (i < f.size) {
        s = (s + " ") + fmt2(f[i]!!)
        i = i + 1
    }
    s = s + "]"
    return s
}

fun user_main(): Unit {
    var inputPoints: MutableList<Point> = mutableListOf(Point(x = 0.0 - 1.0, y = 1.0, z = 1.0), Point(x = 0.0 - 1.0, y = 0.0 - 1.0, z = 1.0), Point(x = 1.0, y = 0.0 - 1.0, z = 1.0), Point(x = 1.0, y = 1.0, z = 1.0), Point(x = 1.0, y = 0.0 - 1.0, z = 0.0 - 1.0), Point(x = 1.0, y = 1.0, z = 0.0 - 1.0), Point(x = 0.0 - 1.0, y = 0.0 - 1.0, z = 0.0 - 1.0), Point(x = 0.0 - 1.0, y = 1.0, z = 0.0 - 1.0))
    var inputFaces: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1, 2, 3), mutableListOf(3, 2, 4, 5), mutableListOf(5, 4, 6, 7), mutableListOf(7, 0, 3, 5), mutableListOf(7, 6, 1, 0), mutableListOf(6, 1, 2, 4))
    var outputPoints: MutableList<Point> = inputPoints
    var outputFaces: MutableList<MutableList<Int>> = inputFaces
    var i: Int = 0
    while (i < 1) {
        var res: MutableList<Any?> = cmcSubdiv(outputPoints, outputFaces)
        outputPoints = ((res[0] as Any?) as MutableList<Point>)
        outputFaces = ((res[1] as Any?) as MutableList<MutableList<Int>>)
        i = i + 1
    }
    for (p in outputPoints) {
        println(formatPoint(p))
    }
    println("")
    for (f in outputFaces) {
        println(formatFace(f))
    }
}

fun main() {
    user_main()
}
