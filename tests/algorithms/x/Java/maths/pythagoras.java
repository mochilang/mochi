public class Main {
    static class Point {
        double x;
        double y;
        double z;
        Point(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        Point() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z));
        }
    }


    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double sqrt_approx(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double distance(Point a, Point b) {
        double dx = (double)((double)(b.x) - (double)(a.x));
        double dy_1 = (double)((double)(b.y) - (double)(a.y));
        double dz_1 = (double)((double)(b.z) - (double)(a.z));
        return sqrt_approx((double)(absf((double)((double)((double)((double)(dx) * (double)(dx)) + (double)((double)(dy_1) * (double)(dy_1))) + (double)((double)(dz_1) * (double)(dz_1))))));
    }

    static String point_to_string(Point p) {
        return "Point(" + _p(p.x) + ", " + _p(p.y) + ", " + _p(p.z) + ")";
    }

    static void test_distance() {
        Point p1 = new Point(2.0, -1.0, 7.0);
        Point p2_1 = new Point(1.0, -3.0, 5.0);
        double d_1 = (double)(distance(p1, p2_1));
        if ((double)(absf((double)((double)(d_1) - (double)(3.0)))) > (double)(0.0001)) {
            throw new RuntimeException(String.valueOf("distance test failed"));
        }
        System.out.println("Distance from " + String.valueOf(point_to_string(p1)) + " to " + String.valueOf(point_to_string(p2_1)) + " is " + _p(d_1));
    }

    static void main() {
        test_distance();
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
