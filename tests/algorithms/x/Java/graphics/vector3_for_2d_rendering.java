public class Main {
    static double PI = (double)(3.141592653589793);

    static double floor(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = (long)((long)(i) - 1L);
        }
        return ((Number)(i)).doubleValue();
    }

    static double modf(double x, double m) {
        return (double)(x) - (double)(Math.floor((double)(x) / (double)(m)) * (double)(m));
    }

    static double sin_taylor(double x) {
        double term = (double)(x);
        double sum_1 = (double)(x);
        long i_2 = 1L;
        while ((long)(i_2) < 10L) {
            double k1_1 = (double)((double)(2.0) * (double)((((Number)(i_2)).doubleValue())));
            double k2_1 = (double)((double)(k1_1) + (double)(1.0));
            term = (double)((double)((double)((double)(-term) * (double)(x)) * (double)(x)) / (double)(((double)(k1_1) * (double)(k2_1))));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            i_2 = (long)((long)(i_2) + 1L);
        }
        return sum_1;
    }

    static double cos_taylor(double x) {
        double term_1 = (double)(1.0);
        double sum_3 = (double)(1.0);
        long i_4 = 1L;
        while ((long)(i_4) < 10L) {
            double k1_3 = (double)((double)((double)(2.0) * (double)((((Number)(i_4)).doubleValue()))) - (double)(1.0));
            double k2_3 = (double)((double)(2.0) * (double)((((Number)(i_4)).doubleValue())));
            term_1 = (double)((double)((double)((double)(-term_1) * (double)(x)) * (double)(x)) / (double)(((double)(k1_3) * (double)(k2_3))));
            sum_3 = (double)((double)(sum_3) + (double)(term_1));
            i_4 = (long)((long)(i_4) + 1L);
        }
        return sum_3;
    }

    static double[] convert_to_2d(double x, double y, double z, double scale, double distance) {
        double projected_x = (double)((double)(((double)(((double)(x) * (double)(distance))) / (double)(((double)(z) + (double)(distance))))) * (double)(scale));
        double projected_y_1 = (double)((double)(((double)(((double)(y) * (double)(distance))) / (double)(((double)(z) + (double)(distance))))) * (double)(scale));
        return new double[]{projected_x, projected_y_1};
    }

    static double[] rotate(double x, double y, double z, String axis, double angle) {
        double[] angle_1 = new double[1];
        angle_1[0] = (double)((double)((double)((double)(modf((double)(angle), (double)(360.0))) / (double)(450.0)) * (double)(180.0)) / (double)(PI));
        angle_1[0] = (double)(modf((double)(angle_1[0]), (double)((double)(2.0) * (double)(PI))));
        if ((double)(angle_1[0]) > (double)(PI)) {
            angle_1[0] = (double)((double)(angle_1[0]) - (double)((double)(2.0) * (double)(PI)));
        }
        if ((axis.equals("z"))) {
            double new_x_1 = (double)((double)((double)(x) * (double)(cos_taylor((double)(angle_1[0])))) - (double)((double)(y) * (double)(sin_taylor((double)(angle_1[0])))));
            double new_y_1 = (double)((double)((double)(y) * (double)(cos_taylor((double)(angle_1[0])))) + (double)((double)(x) * (double)(sin_taylor((double)(angle_1[0])))));
            double new_z_1 = (double)(z);
            return new double[]{new_x_1, new_y_1, new_z_1};
        }
        if ((axis.equals("x"))) {
            double new_y_3 = (double)((double)((double)(y) * (double)(cos_taylor((double)(angle_1[0])))) - (double)((double)(z) * (double)(sin_taylor((double)(angle_1[0])))));
            double new_z_3 = (double)((double)((double)(z) * (double)(cos_taylor((double)(angle_1[0])))) + (double)((double)(y) * (double)(sin_taylor((double)(angle_1[0])))));
            double new_x_3 = (double)(x);
            return new double[]{new_x_3, new_y_3, new_z_3};
        }
        if ((axis.equals("y"))) {
            double new_x_5 = (double)((double)((double)(x) * (double)(cos_taylor((double)(angle_1[0])))) - (double)((double)(z) * (double)(sin_taylor((double)(angle_1[0])))));
            double new_z_5 = (double)((double)((double)(z) * (double)(cos_taylor((double)(angle_1[0])))) + (double)((double)(x) * (double)(sin_taylor((double)(angle_1[0])))));
            double new_y_5 = (double)(y);
            return new double[]{new_x_5, new_y_5, new_z_5};
        }
        System.out.println("not a valid axis, choose one of 'x', 'y', 'z'");
        return new double[]{0.0, 0.0, 0.0};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(convert_to_2d((double)(1.0), (double)(2.0), (double)(3.0), (double)(10.0), (double)(10.0))));
            System.out.println(_p(rotate((double)(1.0), (double)(2.0), (double)(3.0), "y", (double)(90.0))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
