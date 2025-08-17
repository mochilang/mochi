public class Main {
    static class Point3d {
        double x;
        double y;
        double z;
        Point3d(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        Point3d() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z));
        }
    }

    static class Vector3d {
        double x;
        double y;
        double z;
        Vector3d(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        Vector3d() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z));
        }
    }


    static Vector3d create_vector(Point3d p1, Point3d p2) {
        double vx = (double)((double)(p2.x) - (double)(p1.x));
        double vy_1 = (double)((double)(p2.y) - (double)(p1.y));
        double vz_1 = (double)((double)(p2.z) - (double)(p1.z));
        return new Vector3d(vx, vy_1, vz_1);
    }

    static Vector3d get_3d_vectors_cross(Vector3d ab, Vector3d ac) {
        double cx = (double)((double)((double)(ab.y) * (double)(ac.z)) - (double)((double)(ab.z) * (double)(ac.y)));
        double cy_1 = (double)((double)((double)(ab.z) * (double)(ac.x)) - (double)((double)(ab.x) * (double)(ac.z)));
        double cz_1 = (double)((double)((double)(ab.x) * (double)(ac.y)) - (double)((double)(ab.y) * (double)(ac.x)));
        return new Vector3d(cx, cy_1, cz_1);
    }

    static double pow10(long exp) {
        double result = (double)(1.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            result = (double)((double)(result) * (double)(10.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static double round_float(double x, long digits) {
        double factor = (double)(pow10((long)(digits)));
        double v_1 = (double)((double)(x) * (double)(factor));
        if ((double)(v_1) >= (double)(0.0)) {
            v_1 = (double)((double)(v_1) + (double)(0.5));
        } else {
            v_1 = (double)((double)(v_1) - (double)(0.5));
        }
        long t_1 = (long)(((Number)(v_1)).intValue());
        return (double)((((Number)(t_1)).doubleValue())) / (double)(factor);
    }

    static boolean is_zero_vector(Vector3d v, long accuracy) {
        return (double)(round_float((double)(v.x), (long)(accuracy))) == (double)(0.0) && (double)(round_float((double)(v.y), (long)(accuracy))) == (double)(0.0) && (double)(round_float((double)(v.z), (long)(accuracy))) == (double)(0.0);
    }

    static boolean are_collinear(Point3d a, Point3d b, Point3d c, long accuracy) {
        Vector3d ab = create_vector(a, b);
        Vector3d ac_1 = create_vector(a, c);
        Vector3d cross_1 = get_3d_vectors_cross(ab, ac_1);
        return is_zero_vector(cross_1, (long)(accuracy));
    }

    static void test_are_collinear() {
        Point3d p1 = new Point3d(0.0, 0.0, 0.0);
        Point3d p2_1 = new Point3d(1.0, 1.0, 1.0);
        Point3d p3_1 = new Point3d(2.0, 2.0, 2.0);
        if (!(Boolean)are_collinear(p1, p2_1, p3_1, 10L)) {
            throw new RuntimeException(String.valueOf("collinear test failed"));
        }
        Point3d q3_1 = new Point3d(1.0, 2.0, 3.0);
        if (are_collinear(p1, p2_1, q3_1, 10L)) {
            throw new RuntimeException(String.valueOf("non-collinear test failed"));
        }
    }

    static void main() {
        test_are_collinear();
        Point3d a_1 = new Point3d(4.802293498137402, 3.536233125455244, 0.0);
        Point3d b_1 = new Point3d(-2.186788107953106, -9.24561398001649, 7.141509524846482);
        Point3d c_1 = new Point3d(1.530169574640268, -2.447927606600034, 3.343487096469054);
        System.out.println(_p(are_collinear(a_1, b_1, c_1, 10L)));
        Point3d d_1 = new Point3d(2.399001826862445, -2.452009976680793, 4.464656666157666);
        Point3d e_1 = new Point3d(-3.682816335934376, 5.753788986533145, 9.490993909044244);
        Point3d f_1 = new Point3d(1.962903518985307, 3.741415730125627, 7.0);
        System.out.println(_p(are_collinear(d_1, e_1, f_1, 10L)));
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
