public class Main {
    static class Point {
        double x;
        double y;
        Point(double x, double y) {
            this.x = x;
            this.y = y;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static class Triangle {
        Point p1;
        Point p2;
        Point p3;
        Triangle(Point p1, Point p2, Point p3) {
            this.p1 = p1;
            this.p2 = p2;
            this.p3 = p3;
        }
        @Override public String toString() {
            return String.format("{'p1': %s, 'p2': %s, 'p3': %s}", String.valueOf(p1), String.valueOf(p2), String.valueOf(p3));
        }
    }


    static String fmt1(double f) {
        String s = String.valueOf(f);
        int idx = ((Number)(s.indexOf("."))).intValue();
        if (idx < 0) {
            s = s + ".0";
        } else {
            int need = idx + 2;
            if (_runeLen(s) > need) {
                s = _substr(s, 0, need);
            } else {
                while (_runeLen(s) < need) {
                    s = s + "0";
                }
            }
        }
        return s;
    }

    static String pointStr(Point p) {
        return "(" + String.valueOf(fmt1(p.x)) + ", " + String.valueOf(fmt1(p.y)) + ")";
    }

    static String triangleStr(Triangle t) {
        return "Triangle " + String.valueOf(pointStr(t.p1)) + ", " + String.valueOf(pointStr(t.p2)) + ", " + String.valueOf(pointStr(t.p3));
    }

    static double orient(Point a, Point b, Point c) {
        return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
    }

    static boolean pointInTri(Point p, Triangle t, boolean onBoundary) {
        double d1 = orient(p, t.p1, t.p2);
        double d2 = orient(p, t.p2, t.p3);
        double d3 = orient(p, t.p3, t.p1);
        boolean hasNeg = d1 < 0.0 || d2 < 0.0 || d3 < 0.0;
        boolean hasPos = d1 > 0.0 || d2 > 0.0 || d3 > 0.0;
        if (onBoundary) {
            return !(hasNeg && hasPos);
        }
        return !(hasNeg && hasPos) && d1 != 0.0 && d2 != 0.0 && d3 != 0.0;
    }

    static boolean edgeCheck(Point a0, Point a1, Point[] bs, boolean onBoundary) {
        double d0 = orient(a0, a1, bs[0]);
        double d1 = orient(a0, a1, bs[1]);
        double d2 = orient(a0, a1, bs[2]);
        if (onBoundary) {
            return d0 <= 0.0 && d1 <= 0.0 && d2 <= 0.0;
        }
        return d0 < 0.0 && d1 < 0.0 && d2 < 0.0;
    }

    static boolean triTri2D(Triangle t1, Triangle t2, boolean onBoundary) {
        Point[] a = new Point[]{t1.p1, t1.p2, t1.p3};
        Point[] b = new Point[]{t2.p1, t2.p2, t2.p3};
        int i = 0;
        while (i < 3) {
            int j = Math.floorMod((i + 1), 3);
            if (edgeCheck(a[i], a[j], b, onBoundary)) {
                return false;
            }
            i = i + 1;
        }
        i = 0;
        while (i < 3) {
            int j = Math.floorMod((i + 1), 3);
            if (edgeCheck(b[i], b[j], a, onBoundary)) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static String iff(boolean cond, String a, String b) {
        if (cond) {
            return a;
        } else {
            return b;
        }
    }

    static void main() {
        Triangle t1 = new Triangle(new Point(0.0, 0.0), new Point(5.0, 0.0), new Point(0.0, 5.0));
        Triangle t2 = new Triangle(new Point(0.0, 0.0), new Point(5.0, 0.0), new Point(0.0, 6.0));
        System.out.println(String.valueOf(triangleStr(t1)) + " and");
        System.out.println(triangleStr(t2));
        boolean overlapping = triTri2D(t1, t2, true);
        System.out.println(iff(overlapping, "overlap", "do not overlap"));
        System.out.println("");
        t1 = new Triangle(new Point(0.0, 0.0), new Point(0.0, 5.0), new Point(5.0, 0.0));
        t2 = t1;
        System.out.println(String.valueOf(triangleStr(t1)) + " and");
        System.out.println(triangleStr(t2));
        overlapping = triTri2D(t1, t2, true);
        System.out.println(iff(overlapping, "overlap (reversed)", "do not overlap"));
        System.out.println("");
        t1 = new Triangle(new Point(0.0, 0.0), new Point(5.0, 0.0), new Point(0.0, 5.0));
        t2 = new Triangle(new Point(-10.0, 0.0), new Point(-5.0, 0.0), new Point(-1.0, 6.0));
        System.out.println(String.valueOf(triangleStr(t1)) + " and");
        System.out.println(triangleStr(t2));
        overlapping = triTri2D(t1, t2, true);
        System.out.println(iff(overlapping, "overlap", "do not overlap"));
        System.out.println("");
t1.p3 = new Point(2.5, 5.0);
        t2 = new Triangle(new Point(0.0, 4.0), new Point(2.5, -1.0), new Point(5.0, 4.0));
        System.out.println(String.valueOf(triangleStr(t1)) + " and");
        System.out.println(triangleStr(t2));
        overlapping = triTri2D(t1, t2, true);
        System.out.println(iff(overlapping, "overlap", "do not overlap"));
        System.out.println("");
        t1 = new Triangle(new Point(0.0, 0.0), new Point(1.0, 1.0), new Point(0.0, 2.0));
        t2 = new Triangle(new Point(2.0, 1.0), new Point(3.0, 0.0), new Point(3.0, 2.0));
        System.out.println(String.valueOf(triangleStr(t1)) + " and");
        System.out.println(triangleStr(t2));
        overlapping = triTri2D(t1, t2, true);
        System.out.println(iff(overlapping, "overlap", "do not overlap"));
        System.out.println("");
        t2 = new Triangle(new Point(2.0, 1.0), new Point(3.0, -2.0), new Point(3.0, 4.0));
        System.out.println(String.valueOf(triangleStr(t1)) + " and");
        System.out.println(triangleStr(t2));
        overlapping = triTri2D(t1, t2, true);
        System.out.println(iff(overlapping, "overlap", "do not overlap"));
        System.out.println("");
        t1 = new Triangle(new Point(0.0, 0.0), new Point(1.0, 0.0), new Point(0.0, 1.0));
        t2 = new Triangle(new Point(1.0, 0.0), new Point(2.0, 0.0), new Point(1.0, 1.1));
        System.out.println(String.valueOf(triangleStr(t1)) + " and");
        System.out.println(triangleStr(t2));
        System.out.println("which have only a single corner in contact, if boundary points collide");
        overlapping = triTri2D(t1, t2, true);
        System.out.println(iff(overlapping, "overlap", "do not overlap"));
        System.out.println("");
        System.out.println(String.valueOf(triangleStr(t1)) + " and");
        System.out.println(triangleStr(t2));
        System.out.println("which have only a single corner in contact, if boundary points do not collide");
        overlapping = triTri2D(t1, t2, false);
        System.out.println(iff(overlapping, "overlap", "do not overlap"));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
