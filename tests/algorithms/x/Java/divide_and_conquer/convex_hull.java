public class Main {
    static class Point {
        int x;
        int y;
        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }
        Point() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }


    static int cross(Point o, Point a, Point b) {
        return (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
    }

    static Point[] sortPoints(Point[] ps) {
        Point[] arr = ((Point[])(ps));
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

    static Point[] convex_hull(Point[] ps) {
        ps = ((Point[])(sortPoints(((Point[])(ps)))));
        Point[] lower = ((Point[])(new Point[]{}));
        for (Point p : ps) {
            while (lower.length >= 2 && cross(lower[lower.length - 2], lower[lower.length - 1], p) <= 0) {
                lower = ((Point[])(java.util.Arrays.copyOfRange(lower, 0, lower.length - 1)));
            }
            lower = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(lower), java.util.stream.Stream.of(p)).toArray(Point[]::new)));
        }
        Point[] upper = ((Point[])(new Point[]{}));
        int i_1 = ps.length - 1;
        while (i_1 >= 0) {
            Point p_1 = ps[i_1];
            while (upper.length >= 2 && cross(upper[upper.length - 2], upper[upper.length - 1], p_1) <= 0) {
                upper = ((Point[])(java.util.Arrays.copyOfRange(upper, 0, upper.length - 1)));
            }
            upper = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(upper), java.util.stream.Stream.of(p_1)).toArray(Point[]::new)));
            i_1 = i_1 - 1;
        }
        Point[] hull = ((Point[])(java.util.Arrays.copyOfRange(lower, 0, lower.length - 1)));
        int j_1 = 0;
        while (j_1 < upper.length - 1) {
            hull = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(hull), java.util.stream.Stream.of(upper[j_1])).toArray(Point[]::new)));
            j_1 = j_1 + 1;
        }
        return hull;
    }
    public static void main(String[] args) {
    }
}
