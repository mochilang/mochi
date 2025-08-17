public class Main {
    static double PI = (double)(3.141592653589793);

    static double radians(double degree) {
        return (double)(degree) / (double)(((double)(180.0) / (double)(PI)));
    }

    static double abs_float(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static boolean almost_equal(double a, double b) {
        return (double)(abs_float((double)((double)(a) - (double)(b)))) <= (double)(1e-08);
    }

    static void test_radians() {
        if (!(Boolean)almost_equal((double)(radians((double)(180.0))), (double)(PI))) {
            throw new RuntimeException(String.valueOf("radians 180 failed"));
        }
        if (!(Boolean)almost_equal((double)(radians((double)(92.0))), (double)(1.6057029118347832))) {
            throw new RuntimeException(String.valueOf("radians 92 failed"));
        }
        if (!(Boolean)almost_equal((double)(radians((double)(274.0))), (double)(4.782202150464463))) {
            throw new RuntimeException(String.valueOf("radians 274 failed"));
        }
        if (!(Boolean)almost_equal((double)(radians((double)(109.82))), (double)(1.9167205845401725))) {
            throw new RuntimeException(String.valueOf("radians 109.82 failed"));
        }
    }

    static void main() {
        test_radians();
        System.out.println(_p(radians((double)(180.0))));
        System.out.println(_p(radians((double)(92.0))));
        System.out.println(_p(radians((double)(274.0))));
        System.out.println(_p(radians((double)(109.82))));
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
