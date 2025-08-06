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

    static double sin_taylor(double angle) {
        double x = modf(angle, 2.0 * PI);
        if (x > PI) {
            x = x - 2.0 * PI;
        }
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

    static double cos_taylor(double angle) {
        double x_1 = modf(angle, 2.0 * PI);
        if (x_1 > PI) {
            x_1 = x_1 - 2.0 * PI;
        }
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int i_2 = 1;
        while (i_2 < 10) {
            double k1_1 = 2.0 * (((Number)(i_2)).doubleValue()) - 1.0;
            double k2_1 = 2.0 * (((Number)(i_2)).doubleValue());
            term_1 = -term_1 * x_1 * x_1 / (k1_1 * k2_1);
            sum_1 = sum_1 + term_1;
            i_2 = i_2 + 1;
        }
        return sum_1;
    }

    static String matrix_to_string(double[][] m) {
        String s = "[";
        int i_3 = 0;
        while (i_3 < m.length) {
            double[] row = ((double[])(m[i_3]));
            s = s + "[";
            int j = 0;
            while (j < row.length) {
                s = s + _p(_geto(row, j));
                if (j < row.length - 1) {
                    s = s + ", ";
                }
                j = j + 1;
            }
            s = s + "]";
            if (i_3 < m.length - 1) {
                s = s + ", ";
            }
            i_3 = i_3 + 1;
        }
        s = s + "]";
        return s;
    }

    static double[][] scaling(double f) {
        return new Object[][]{new Object[]{f, 0.0}, new Object[]{0.0, f}};
    }

    static double[][] rotation(double angle) {
        double c = cos_taylor(angle);
        double s_1 = sin_taylor(angle);
        return new double[][]{new double[]{c, -s_1}, new double[]{s_1, c}};
    }

    static double[][] projection(double angle) {
        double c_1 = cos_taylor(angle);
        double s_2 = sin_taylor(angle);
        double cs = c_1 * s_2;
        return new double[][]{new double[]{c_1 * c_1, cs}, new double[]{cs, s_2 * s_2}};
    }

    static double[][] reflection(double angle) {
        double c_2 = cos_taylor(angle);
        double s_3 = sin_taylor(angle);
        double cs_1 = c_2 * s_3;
        return new double[][]{new double[]{2.0 * c_2 - 1.0, 2.0 * cs_1}, new double[]{2.0 * cs_1, 2.0 * s_3 - 1.0}};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            System.out.println("    scaling(5) = " + String.valueOf(matrix_to_string(((double[][])(scaling(5.0))))));
            System.out.println("  rotation(45) = " + String.valueOf(matrix_to_string(((double[][])(rotation(45.0))))));
            System.out.println("projection(45) = " + String.valueOf(matrix_to_string(((double[][])(projection(45.0))))));
            System.out.println("reflection(45) = " + String.valueOf(matrix_to_string(((double[][])(reflection(45.0))))));
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
