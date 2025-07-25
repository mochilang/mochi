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

    static class QuadSpline {
        double c0;
        double c1;
        double c2;
        QuadSpline(double c0, double c1, double c2) {
            this.c0 = c0;
            this.c1 = c1;
            this.c2 = c2;
        }
        @Override public String toString() {
            return String.format("{'c0': %s, 'c1': %s, 'c2': %s}", String.valueOf(c0), String.valueOf(c1), String.valueOf(c2));
        }
    }

    static class QuadCurve {
        QuadSpline x;
        QuadSpline y;
        QuadCurve(QuadSpline x, QuadSpline y) {
            this.x = x;
            this.y = y;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static class Data1 {
        Object p;
        Object q;
        Data1(Object p, Object q) {
            this.p = p;
            this.q = q;
        }
        @Override public String toString() {
            return String.format("{'p': %s, 'q': %s}", String.valueOf(p), String.valueOf(q));
        }
    }


    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double maxf(double a, double b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static double minf(double a, double b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static double max3(double a, double b, double c) {
        double m = a;
        if (b > m) {
            m = b;
        }
        if (c > m) {
            m = c;
        }
        return m;
    }

    static double min3(double a, double b, double c) {
        double m = a;
        if (b < m) {
            m = b;
        }
        if (c < m) {
            m = c;
        }
        return m;
    }

    static QuadSpline[] subdivideQuadSpline(QuadSpline q, double t) {
        double s = 1.0 - t;
        QuadSpline u = new QuadSpline(q.c0, 0.0, 0.0);
        QuadSpline v = new QuadSpline(0.0, 0.0, q.c2);
u.c1 = s * q.c0 + t * q.c1;
v.c1 = s * q.c1 + t * q.c2;
u.c2 = s * ((Number)(u.c1)).doubleValue() + t * ((Number)(v.c1)).doubleValue();
v.c0 = u.c2;
        return new QuadSpline[]{u, v};
    }

    static QuadCurve[] subdivideQuadCurve(QuadCurve q, double t) {
        QuadSpline[] xs = subdivideQuadSpline(q.x, t);
        QuadSpline[] ys = subdivideQuadSpline(q.y, t);
        QuadCurve u = new QuadCurve(xs[0], ys[0]);
        QuadCurve v = new QuadCurve(xs[1], ys[1]);
        return new QuadCurve[]{u, v};
    }

    static boolean rectsOverlap(double xa0, double ya0, double xa1, double ya1, double xb0, double yb0, double xb1, double yb1) {
        return xb0 <= xa1 && xa0 <= xb1 && yb0 <= ya1 && ya0 <= yb1;
    }

    static java.util.Map<String,Object> testIntersect(QuadCurve p, QuadCurve q, double tol) {
        double pxmin = min3(p.x.c0, p.x.c1, p.x.c2);
        double pymin = min3(p.y.c0, p.y.c1, p.y.c2);
        double pxmax = max3(p.x.c0, p.x.c1, p.x.c2);
        double pymax = max3(p.y.c0, p.y.c1, p.y.c2);
        double qxmin = min3(q.x.c0, q.x.c1, q.x.c2);
        double qymin = min3(q.y.c0, q.y.c1, q.y.c2);
        double qxmax = max3(q.x.c0, q.x.c1, q.x.c2);
        double qymax = max3(q.y.c0, q.y.c1, q.y.c2);
        boolean exclude = true;
        boolean accept = false;
        Point inter = new Point(0.0, 0.0);
        if (rectsOverlap(pxmin, pymin, pxmax, pymax, qxmin, qymin, qxmax, qymax)) {
            exclude = false;
            double xmin = maxf(pxmin, qxmin);
            double xmax = minf(pxmax, qxmax);
            if (xmax - xmin <= tol) {
                double ymin = maxf(pymin, qymin);
                double ymax = minf(pymax, qymax);
                if (ymax - ymin <= tol) {
                    accept = true;
inter.x = 0.5 * (xmin + xmax);
inter.y = 0.5 * (ymin + ymax);
                }
            }
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("exclude", exclude), java.util.Map.entry("accept", accept), java.util.Map.entry("intersect", inter)));
    }

    static boolean seemsToBeDuplicate(Point[] pts, Point xy, double spacing) {
        int i = 0;
        while (i < pts.length) {
            Point pt = pts[i];
            if (absf(((Number)(pt.x)).doubleValue() - xy.x) < spacing && absf(((Number)(pt.y)).doubleValue() - xy.y) < spacing) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static Point[] findIntersects(QuadCurve p, QuadCurve q, double tol, double spacing) {
        Point[] inters = new Point[]{};
        Data1[] workload = new Data1[]{new Data1(p, q)};
        while (workload.length > 0) {
            int idx = workload.length - 1;
            Object>[] work = workload[idx];
            workload = java.util.Arrays.copyOfRange(workload, 0, idx);
            java.util.Map<String,Object> res = testIntersect(work["p"], work["q"], tol);
            boolean excl = (boolean)(((boolean)res.getOrDefault("exclude", false)));
            boolean acc = (boolean)(((boolean)res.getOrDefault("accept", false)));
            Point inter = (Point)(((Point)res.get("intersect")));
            if (acc) {
                if (!(Boolean)seemsToBeDuplicate(inters, inter, spacing)) {
                    inters = java.util.stream.Stream.concat(java.util.Arrays.stream(inters), java.util.stream.Stream.of(inter)).toArray(Point[]::new);
                }
            } else             if (!excl) {
                QuadCurve[] ps = subdivideQuadCurve(work["p"], 0.5);
                QuadCurve[] qs = subdivideQuadCurve(work["q"], 0.5);
                QuadCurve p0 = ps[0];
                QuadCurve p1 = ps[1];
                QuadCurve q0 = qs[0];
                QuadCurve q1 = qs[1];
                workload = java.util.stream.Stream.concat(java.util.Arrays.stream(workload), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("p", p0), java.util.Map.entry("q", q0))))).toArray(java.util.Map<String,Object>[]::new);
                workload = java.util.stream.Stream.concat(java.util.Arrays.stream(workload), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("p", p0), java.util.Map.entry("q", q1))))).toArray(java.util.Map<String,Object>[]::new);
                workload = java.util.stream.Stream.concat(java.util.Arrays.stream(workload), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("p", p1), java.util.Map.entry("q", q0))))).toArray(java.util.Map<String,Object>[]::new);
                workload = java.util.stream.Stream.concat(java.util.Arrays.stream(workload), java.util.stream.Stream.of(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("p", p1), java.util.Map.entry("q", q1))))).toArray(java.util.Map<String,Object>[]::new);
            }
        }
        return inters;
    }

    static void main() {
        QuadCurve p = new QuadCurve(new QuadSpline(-1.0, 0.0, 1.0), new QuadSpline(0.0, 10.0, 0.0));
        QuadCurve q = new QuadCurve(new QuadSpline(2.0, -8.0, 2.0), new QuadSpline(1.0, 2.0, 3.0));
        double tol = 1e-07;
        double spacing = tol * 10.0;
        Point[] inters = findIntersects(p, q, tol, spacing);
        int i = 0;
        while (i < inters.length) {
            Point pt = inters[i];
            System.out.println(String.valueOf(String.valueOf(String.valueOf("(" + String.valueOf(pt.x)) + ", ") + String.valueOf(pt.y)) + ")");
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
