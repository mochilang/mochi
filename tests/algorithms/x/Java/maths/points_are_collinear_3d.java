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
        double vx = p2.x - p1.x;
        double vy = p2.y - p1.y;
        double vz = p2.z - p1.z;
        return new Vector3d(vx, vy, vz);
    }

    static Vector3d get_3d_vectors_cross(Vector3d ab, Vector3d ac) {
        double cx = ab.y * ac.z - ab.z * ac.y;
        double cy = ab.z * ac.x - ab.x * ac.z;
        double cz = ab.x * ac.y - ab.y * ac.x;
        return new Vector3d(cx, cy, cz);
    }

    static double pow10(int exp) {
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * 10.0;
            i = i + 1;
        }
        return result;
    }

    static double round_float(double x, int digits) {
        double factor = pow10(digits);
        double v = x * factor;
        if (v >= 0.0) {
            v = v + 0.5;
        } else {
            v = v - 0.5;
        }
        int t = ((Number)(v)).intValue();
        return (((Number)(t)).doubleValue()) / factor;
    }

    static boolean is_zero_vector(Vector3d v, int accuracy) {
        return round_float(v.x, accuracy) == 0.0 && round_float(v.y, accuracy) == 0.0 && round_float(v.z, accuracy) == 0.0;
    }

    static boolean are_collinear(Point3d a, Point3d b, Point3d c, int accuracy) {
        Vector3d ab = create_vector(a, b);
        Vector3d ac = create_vector(a, c);
        Vector3d cross = get_3d_vectors_cross(ab, ac);
        return is_zero_vector(cross, accuracy);
    }

    static void test_are_collinear() {
        Point3d p1 = new Point3d(0.0, 0.0, 0.0);
        Point3d p2 = new Point3d(1.0, 1.0, 1.0);
        Point3d p3 = new Point3d(2.0, 2.0, 2.0);
        if (!(Boolean)are_collinear(p1, p2, p3, 10)) {
            throw new RuntimeException(String.valueOf("collinear test failed"));
        }
        Point3d q3 = new Point3d(1.0, 2.0, 3.0);
        if (((Boolean)(are_collinear(p1, p2, q3, 10)))) {
            throw new RuntimeException(String.valueOf("non-collinear test failed"));
        }
    }

    static void main() {
        test_are_collinear();
        Point3d a = new Point3d(4.802293498137402, 3.536233125455244, 0.0);
        Point3d b = new Point3d(-2.186788107953106, -9.24561398001649, 7.141509524846482);
        Point3d c = new Point3d(1.530169574640268, -2.447927606600034, 3.343487096469054);
        System.out.println(_p(are_collinear(a, b, c, 10)));
        Point3d d = new Point3d(2.399001826862445, -2.452009976680793, 4.464656666157666);
        Point3d e = new Point3d(-3.682816335934376, 5.753788986533145, 9.490993909044244);
        Point3d f = new Point3d(1.962903518985307, 3.741415730125627, 7.0);
        System.out.println(_p(are_collinear(d, e, f, 10)));
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
        return String.valueOf(v);
    }
}
