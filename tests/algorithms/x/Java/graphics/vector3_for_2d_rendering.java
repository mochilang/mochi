public class Main {
    static double PI;

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double modf(double x, double m) {
        return x - floor(x / m) * m;
    }

    static double sin_taylor(double x) {
        double term = x;
        double sum = x;
        int i_1 = 1;
        while (i_1 < 10) {
            double k1 = 2.0 * (((Number)(i_1)).doubleValue());
            double k2 = k1 + 1.0;
            term = -term * x * x / (k1 * k2);
            sum = sum + term;
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static double cos_taylor(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int i_2 = 1;
        while (i_2 < 10) {
            double k1_1 = 2.0 * (((Number)(i_2)).doubleValue()) - 1.0;
            double k2_1 = 2.0 * (((Number)(i_2)).doubleValue());
            term_1 = -term_1 * x * x / (k1_1 * k2_1);
            sum_1 = sum_1 + term_1;
            i_2 = i_2 + 1;
        }
        return sum_1;
    }

    static double[] convert_to_2d(double x, double y, double z, double scale, double distance) {
        double projected_x = ((x * distance) / (z + distance)) * scale;
        double projected_y = ((y * distance) / (z + distance)) * scale;
        return new double[]{projected_x, projected_y};
    }

    static double[] rotate(double x, double y, double z, String axis, double angle) {
        double[] angle = new double[1];
        angle[0] = modf(angle[0], 360.0) / 450.0 * 180.0 / PI;
        angle[0] = modf(angle[0], 2.0 * PI);
        if (angle[0] > PI) {
            angle[0] = angle[0] - 2.0 * PI;
        }
        if ((axis.equals("z"))) {
            double new_x = x * cos_taylor(angle[0]) - y * sin_taylor(angle[0]);
            double new_y = y * cos_taylor(angle[0]) + x * sin_taylor(angle[0]);
            double new_z = z;
            return new double[]{new_x, new_y, new_z};
        }
        if ((axis.equals("x"))) {
            double new_y_1 = y * cos_taylor(angle[0]) - z * sin_taylor(angle[0]);
            double new_z_1 = z * cos_taylor(angle[0]) + y * sin_taylor(angle[0]);
            double new_x_1 = x;
            return new double[]{new_x_1, new_y_1, new_z_1};
        }
        if ((axis.equals("y"))) {
            double new_x_2 = x * cos_taylor(angle[0]) - z * sin_taylor(angle[0]);
            double new_z_2 = z * cos_taylor(angle[0]) + x * sin_taylor(angle[0]);
            double new_y_2 = y;
            return new double[]{new_x_2, new_y_2, new_z_2};
        }
        System.out.println("not a valid axis, choose one of 'x', 'y', 'z'");
        return new double[]{0.0, 0.0, 0.0};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            System.out.println(_p(convert_to_2d(1.0, 2.0, 3.0, 10.0, 10.0)));
            System.out.println(_p(rotate(1.0, 2.0, 3.0, "y", 90.0)));
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
