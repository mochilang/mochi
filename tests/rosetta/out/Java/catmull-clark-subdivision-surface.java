// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// catmull-clark-subdivision-surface.mochi
import java.util.*;

class Edge {
    int pn1;
    int pn2;
    int fn1;
    int fn2;
    Point cp;
    Edge(int pn1, int pn2, int fn1, int fn2, Point cp) {
        this.pn1 = pn1;
        this.pn2 = pn2;
        this.fn1 = fn1;
        this.fn2 = fn2;
        this.cp = cp;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Edge other)) return false;
        return Objects.equals(this.pn1, other.pn1) && Objects.equals(this.pn2, other.pn2) && Objects.equals(this.fn1, other.fn1) && Objects.equals(this.fn2, other.fn2) && Objects.equals(this.cp, other.cp);
    }
    @Override public int hashCode() {
        return Objects.hash(pn1, pn2, fn1, fn2, cp);
    }
}
class PointEx {
    Point p;
    int n;
    PointEx(Point p, int n) {
        this.p = p;
        this.n = n;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PointEx other)) return false;
        return Objects.equals(this.p, other.p) && Objects.equals(this.n, other.n);
    }
    @Override public int hashCode() {
        return Objects.hash(p, n);
    }
}
class Point {
    double x;
    double y;
    double z;
    Point(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Point other)) return false;
        return Objects.equals(this.x, other.x) && Objects.equals(this.y, other.y) && Objects.equals(this.z, other.z);
    }
    @Override public int hashCode() {
        return Objects.hash(x, y, z);
    }
}
public class CatmullClarkSubdivisionSurface {
    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if (Objects.equals(s.substring(i, i + 1), ch)) {
                return i;
            }
            i = (int)(i + 1);
        }
        return -1;
    }
    static String fmt4(double x) {
        double y = x * 10000.000000;
        if (y >= 0) {
            y = y + 0.500000;
        }
        else {
            y = y - 0.500000;
        }
        y = Double.parseDouble(String.valueOf((Integer.parseInt(y)))) / 10000.000000;
        String s = String.valueOf(y);
        int dot = indexOf(s, ".");
        if (Objects.equals(dot, 0 - 1)) {
            s = s + ".0000";
        }
        else {
            int decs = s.length() - dot - 1;
            if (decs > 4) {
                s = s.substring(0, dot + 5);
            }
            else {
                while (decs < 4) {
                    s = s + "0";
                    decs = (int)(decs + 1);
                }
            }
        }
        if (x >= 0.000000) {
            s = " " + s;
        }
        return s;
    }
    static String fmt2(int n) {
        String s = String.valueOf(n);
        if (s.length() < 2) {
            return " " + s;
        }
        return s;
    }
    static Point sumPoint(Point p1, Point p2) {
        return new Point(p1.x + p2.x, p1.y + p2.y, p1.z + p2.z);
    }
    static Point mulPoint(Point p, double m) {
        return new Point(p.x * m, p.y * m, p.z * m);
    }
    static Point divPoint(Point p, double d) {
        return mulPoint(p, 1.000000 / d);
    }
    static Point centerPoint(Point p1, Point p2) {
        return divPoint(sumPoint(p1, p2), 2.000000);
    }
    static List<Point> getFacePoints(List<Point> points, List<List<Integer>> faces) {
        List<Point> facePoints = Arrays.asList();
        int i = 0;
        while (i < faces.size()) {
            List<List<Integer>> face = faces.get(i);
            Point fp = new Point(0.000000, 0.000000, 0.000000);
            for (List<Integer> idx : face) {
                fp = sumPoint(fp, points.get(idx));
            }
            fp = divPoint(fp, (Double.parseDouble(String.valueOf(face.size()))));
            facePoints.add(fp);
            i = (int)(i + 1);
        }
        return facePoints;
    }
    static List<List<Integer>> sortEdges(List<List<Integer>> edges) {
        List<List<Integer>> res = Arrays.asList();
        List<List<Integer>> tmp = edges;
        while (tmp.size() > 0) {
            List<List<Integer>> min = tmp.get(0);
            int idx = 0;
            int j = 1;
            while (j < tmp.size()) {
                List<List<Integer>> e = tmp.get(j);
                if (String.valueOf(e.get(0)).compareTo(String.valueOf(min.get(0))) < 0 || (Objects.equals(e.get(0), min.get(0)) && (String.valueOf(e.get(1)).compareTo(String.valueOf(min.get(1))) < 0 || (Objects.equals(e.get(1), min.get(1)) && String.valueOf(e.get(2)).compareTo(String.valueOf(min.get(2))) < 0)))) {
                    min = e;
                    idx = (int)(j);
                }
                j = (int)(j + 1);
            }
            res.add(min);
            List<List<Integer>> out = Arrays.asList();
            int k = 0;
            while (k < tmp.size()) {
                if (k != idx) {
                    out.add(tmp.get(k));
                }
                k = (int)(k + 1);
            }
            tmp = out;
        }
        return res;
    }
    static List<Edge> getEdgesFaces(List<Point> points, List<List<Integer>> faces) {
        List<List<Integer>> edges = Arrays.asList();
        int fnum = 0;
        while (fnum < faces.size()) {
            List<List<Integer>> face = faces.get(fnum);
            int numP = face.size();
            int pi = 0;
            while (pi < numP) {
                List<List<Integer>> pn1 = face.get(pi);
                int pn2 = 0;
                if (pi < numP - 1) {
                    pn2 = (int)(face.get(pi + 1));
                }
                else {
                    pn2 = (int)(face.get(0));
                }
                if (pn1 > pn2) {
                    List<List<Integer>> tmpn = pn1;
                    pn1 = pn2;
                    pn2 = (int)(tmpn);
                }
                edges.add(Arrays.asList(pn1, pn2, fnum));
                pi = (int)(pi + 1);
            }
            fnum = (int)(fnum + 1);
        }
        edges = sortEdges(edges);
        List<List<Integer>> merged = Arrays.asList();
        int idx = 0;
        while (idx < edges.size()) {
            List<List<Integer>> e1 = edges.get(idx);
            if (idx < edges.size() - 1) {
                List<List<Integer>> e2 = edges.get(idx + 1);
                if (Objects.equals(e1.get(0), e2.get(0)) && Objects.equals(e1.get(1), e2.get(1))) {
                    merged.add(Arrays.asList(e1.get(0), e1.get(1), e1.get(2), e2.get(2)));
                    idx = (int)(idx + 2);
                    continue;
                }
            }
            merged.add(Arrays.asList(e1.get(0), e1.get(1), e1.get(2), -1));
            idx = (int)(idx + 1);
        }
        List<Edge> edgesCenters = Arrays.asList();
        for (List<Integer> me : merged) {
            List<Point> p1 = points.get(me.get(0));
            List<Point> p2 = points.get(me.get(1));
            Point cp = centerPoint(p1, p2);
            edgesCenters.add(new Edge(me.get(0), me.get(1), me.get(2), me.get(3), cp));
        }
        return edgesCenters;
    }
    static List<Point> getEdgePoints(List<Point> points, List<Edge> edgesFaces, List<Point> facePoints) {
        List<Point> edgePoints = Arrays.asList();
        int i = 0;
        while (i < edgesFaces.size()) {
            List<Edge> edge = edgesFaces.get(i);
            Object cp = edge.cp;
            List<Point> fp1 = facePoints.get(edge.fn1);
            List<Point> fp2 = fp1;
            if (!Objects.equals(edge.fn2, 0 - 1)) {
                fp2 = facePoints.get(edge.fn2);
            }
            Point cfp = centerPoint(fp1, fp2);
            edgePoints.add(centerPoint(cp, cfp));
            i = (int)(i + 1);
        }
        return edgePoints;
    }
    static List<Point> getAvgFacePoints(List<Point> points, List<List<Integer>> faces, List<Point> facePoints) {
        int numP = points.size();
        List<PointEx> temp = Arrays.asList();
        int i = 0;
        while (i < numP) {
            temp.add(new PointEx(new Point(0.000000, 0.000000, 0.000000), 0));
            i = (int)(i + 1);
        }
        int fnum = 0;
        while (fnum < faces.size()) {
            List<Point> fp = facePoints.get(fnum);
            for (List<Integer> pn : (List<List<Integer>>)faces.get(fnum)) {
                List<PointEx> tp = temp.get(pn);
                temp.set(pn, new PointEx(sumPoint(tp.p, fp), tp.n + 1));
            }
            fnum = (int)(fnum + 1);
        }
        List<Point> avg = Arrays.asList();
        int j = 0;
        while (j < numP) {
            List<PointEx> tp = temp.get(j);
            avg.add(divPoint(tp.p, Double.parseDouble(String.valueOf(tp.n))));
            j = (int)(j + 1);
        }
        return avg;
    }
    static List<Point> getAvgMidEdges(List<Point> points, List<Edge> edgesFaces) {
        int numP = points.size();
        List<PointEx> temp = Arrays.asList();
        int i = 0;
        while (i < numP) {
            temp.add(new PointEx(new Point(0.000000, 0.000000, 0.000000), 0));
            i = (int)(i + 1);
        }
        for (Edge edge : edgesFaces) {
            Point cp = edge.cp;
            List<Integer> arr = new ArrayList<>(Arrays.asList(edge.pn1, edge.pn2));
            for (Integer pn : arr) {
                List<PointEx> tp = temp.get(pn);
                temp.set(pn, new PointEx(sumPoint(tp.p, cp), tp.n + 1));
            }
        }
        List<Point> avg = Arrays.asList();
        int j = 0;
        while (j < numP) {
            List<PointEx> tp = temp.get(j);
            avg.add(divPoint(tp.p, Double.parseDouble(String.valueOf(tp.n))));
            j = (int)(j + 1);
        }
        return avg;
    }
    static List<Integer> getPointsFaces(List<Point> points, List<List<Integer>> faces) {
        List<Integer> pf = Arrays.asList();
        int i = 0;
        while (i < points.size()) {
            pf.add(0);
            i = (int)(i + 1);
        }
        int fnum = 0;
        while (fnum < faces.size()) {
            for (List<Integer> pn : (List<List<Integer>>)faces.get(fnum)) {
                pf.set(pn, ((Number)pf.get(pn)).doubleValue() + 1);
            }
            fnum = (int)(fnum + 1);
        }
        return pf;
    }
    static List<Point> getNewPoints(List<Point> points, List<Integer> pf, List<Point> afp, List<Point> ame) {
        List<Point> newPts = Arrays.asList();
        int i = 0;
        while (i < points.size()) {
            double n = Double.parseDouble(String.valueOf(((List)pf.get(i))));
            double m1 = (n - 3.000000) / n;
            double m2 = 1.000000 / n;
            double m3 = 2.000000 / n;
            List<Point> old = points.get(i);
            Point p1 = mulPoint(old, m1);
            Point p2 = mulPoint(afp.get(i), m2);
            Point p3 = mulPoint(ame.get(i), m3);
            newPts.add(sumPoint(sumPoint(p1, p2), p3));
            i = (int)(i + 1);
        }
        return newPts;
    }
    static String key(int a, int b) {
        if (a < b) {
            return String.valueOf(a) + "," + String.valueOf(b);
        }
        return String.valueOf(b) + "," + String.valueOf(a);
    }
    static List<Object> cmcSubdiv(List<Point> points, List<List<Integer>> faces) {
        List<Point> facePoints = getFacePoints(points, faces);
        List<Edge> edgesFaces = getEdgesFaces(points, faces);
        List<Point> edgePoints = getEdgePoints(points, edgesFaces, facePoints);
        List<Point> avgFacePoints = getAvgFacePoints(points, faces, facePoints);
        List<Point> avgMidEdges = getAvgMidEdges(points, edgesFaces);
        List<Integer> pointsFaces = getPointsFaces(points, faces);
        List<Point> newPoints = getNewPoints(points, pointsFaces, avgFacePoints, avgMidEdges);
        List<Integer> facePointNums = Arrays.asList();
        int nextPoint = newPoints.size();
        for (Point fp : facePoints) {
            newPoints.add(fp);
            facePointNums.add(nextPoint);
            nextPoint = (int)(nextPoint + 1);
        }
        Map<String,Integer> edgePointNums = new LinkedHashMap<String,Integer>();
        int idx = 0;
        while (idx < edgesFaces.size()) {
            List<Edge> e = edgesFaces.get(idx);
            newPoints.add(edgePoints.get(idx));
            edgePointNums.put(key(e.pn1, e.pn2), nextPoint);
            nextPoint = (int)(nextPoint + 1);
            idx = (int)(idx + 1);
        }
        List<List<Integer>> newFaces = Arrays.asList();
        int fnum = 0;
        while (fnum < faces.size()) {
            List<List<Integer>> oldFace = faces.get(fnum);
            if (Objects.equals(oldFace.size(), 4)) {
                List<List<Integer>> a = oldFace.get(0);
                List<List<Integer>> b = oldFace.get(1);
                List<List<Integer>> c = oldFace.get(2);
                List<List<Integer>> d = oldFace.get(3);
                List<Integer> fpnum = facePointNums.get(fnum);
                Map<String,Integer> ab = edgePointNums.get(key(a, b));
                Map<String,Integer> da = edgePointNums.get(key(d, a));
                Map<String,Integer> bc = edgePointNums.get(key(b, c));
                Map<String,Integer> cd = edgePointNums.get(key(c, d));
                newFaces.add(Arrays.asList(a, ab, fpnum, da));
                newFaces.add(Arrays.asList(b, bc, fpnum, ab));
                newFaces.add(Arrays.asList(c, cd, fpnum, bc));
                newFaces.add(Arrays.asList(d, da, fpnum, cd));
            }
            fnum = (int)(fnum + 1);
        }
        return Arrays.asList(newPoints, newFaces);
    }
    static String formatPoint(Point p) {
        return "[" + fmt4(p.x) + " " + fmt4(p.y) + " " + fmt4(p.z) + "]";
    }
    static String formatFace(List<Integer> f) {
        if (Objects.equals(f.size(), 0)) {
            return "[]";
        }
        String s = "[" + ((Number)fmt2(f.get(0))).doubleValue();
        int i = 1;
        while (i < f.size()) {
            s = s + " " + ((Number)fmt2(f.get(i))).doubleValue();
            i = (int)(i + 1);
        }
        s = s + "]";
        return s;
    }
    static void main() {
        List<Point> inputPoints = new ArrayList<>(Arrays.asList(new Point(-1.000000, 1.000000, 1.000000), new Point(-1.000000, -1.000000, 1.000000), new Point(1.000000, -1.000000, 1.000000), new Point(1.000000, 1.000000, 1.000000), new Point(1.000000, -1.000000, -1.000000), new Point(1.000000, 1.000000, -1.000000), new Point(-1.000000, -1.000000, -1.000000), new Point(-1.000000, 1.000000, -1.000000)));
        List<List<Integer>> inputFaces = new ArrayList<>(Arrays.asList(Arrays.asList(0, 1, 2, 3), Arrays.asList(3, 2, 4, 5), Arrays.asList(5, 4, 6, 7), Arrays.asList(7, 0, 3, 5), Arrays.asList(7, 6, 1, 0), Arrays.asList(6, 1, 2, 4)));
        List<Point> outputPoints = inputPoints;
        List<List<Integer>> outputFaces = inputFaces;
        int i = 0;
        while (i < 1) {
            List<Object> res = cmcSubdiv(outputPoints, outputFaces);
            outputPoints = res.get(0);
            outputFaces = res.get(1);
            i = (int)(i + 1);
        }
        for (Point p : outputPoints) {
            System.out.println(formatPoint(p));
        }
        System.out.println("");
        for (List<Integer> f : outputFaces) {
            System.out.println(formatFace(f));
        }
    }
    public static void main(String[] args) {
        main();
    }
}
