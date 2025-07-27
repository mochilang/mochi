public class Main {
    static class Point {
        int x;
        int y;
        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static Point[] pts = new Point[]{new Point(16, 3), new Point(12, 17), new Point(0, 6), new Point(-4, -6), new Point(16, 6), new Point(16, -7), new Point(16, -3), new Point(17, -4), new Point(5, 19), new Point(19, -8), new Point(3, 16), new Point(12, 13), new Point(3, -4), new Point(17, 5), new Point(-3, 15), new Point(-3, -9), new Point(0, 11), new Point(-9, -3), new Point(-4, -2), new Point(12, 10)};
    static Point[] hull = convexHull(pts);

    static boolean ccw(Point a, Point b, Point c) {
        int lhs = (b.x - a.x) * (c.y - a.y);
        int rhs = (b.y - a.y) * (c.x - a.x);
        return lhs > rhs;
    }

    static Point[] sortPoints(Point[] ps) {
        Point[] arr = ps;
        int n = arr.length;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < n - 1) {
                Point p = arr[j];
                Point q = arr[j + 1];
                if (p.x > q.x || (p.x == q.x && p.y > q.y)) {
arr[j] = q;
arr[j + 1] = p;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static Point[] convexHull(Point[] ps) {
        ps = sortPoints(ps);
        Point[] h = new Point[]{};
        for (Point pt : ps) {
            while (h.length >= 2 && ccw(h[h.length - 2], h[h.length - 1], pt) == false) {
                h = java.util.Arrays.copyOfRange(h, 0, h.length - 1);
            }
            h = java.util.stream.Stream.concat(java.util.Arrays.stream(h), java.util.stream.Stream.of(pt)).toArray(Point[]::new);
        }
        int i = ps.length - 2;
        int t = h.length + 1;
        while (i >= 0) {
            Point pt = ps[i];
            while (h.length >= t && ccw(h[h.length - 2], h[h.length - 1], pt) == false) {
                h = java.util.Arrays.copyOfRange(h, 0, h.length - 1);
            }
            h = java.util.stream.Stream.concat(java.util.Arrays.stream(h), java.util.stream.Stream.of(pt)).toArray(Point[]::new);
            i = i - 1;
        }
        return java.util.Arrays.copyOfRange(h, 0, h.length - 1);
    }

    static String pointStr(Point p) {
        return "(" + String.valueOf(p.x) + "," + String.valueOf(p.y) + ")";
    }

    static String hullStr(Point[] h) {
        String s = "[";
        int i = 0;
        while (i < h.length) {
            s = s + String.valueOf(pointStr(h[i]));
            if (i < h.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        System.out.println("Convex Hull: " + String.valueOf(hullStr(hull)));
    }
}
