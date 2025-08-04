public class Main {
    static double[] xs;

    static double ln(double x) {
        double k = 0.0;
        double v = x;
        while (v >= 2.0) {
            v = v / 2.0;
            k = k + 1.0;
        }
        while (v < 1.0) {
            v = v * 2.0;
            k = k - 1.0;
        }
        double z = (v - 1.0) / (v + 1.0);
        double zpow = z;
        double sum = z;
        int i = 3;
        while (i <= 9) {
            zpow = zpow * z * z;
            sum = sum + zpow / (((Number)(i)).doubleValue());
            i = i + 2;
        }
        double ln2 = 0.6931471805599453;
        return (k * ln2) + 2.0 * sum;
    }

    static double expf(double x) {
        double term = 1.0;
        double sum_1 = 1.0;
        int i_1 = 1;
        while (i_1 < 20) {
            term = term * x / ((Number)(i_1)).doubleValue();
            sum_1 = sum_1 + term;
            i_1 = i_1 + 1;
        }
        return sum_1;
    }

    static double powf(double base, double exp) {
        return expf(exp * ln(base));
    }

    static double lanczos7(double z) {
        double t = z + 6.5;
        double x = 0.9999999999998099 + 676.5203681218851 / z - 1259.1392167224028 / (z + 1.0) + 771.3234287776531 / (z + 2.0) - 176.6150291621406 / (z + 3.0) + 12.507343278686905 / (z + 4.0) - 0.13857109526572012 / (z + 5.0) + 9.984369578019572e-06 / (z + 6.0) + 1.5056327351493116e-07 / (z + 7.0);
        return 2.5066282746310002 * powf(t, z - 0.5) * powf(2.718281828459045, -t) * x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            xs = ((double[])(new double[]{-0.5, 0.1, 0.5, 1.0, 1.5, 2.0, 3.0, 10.0, 140.0, 170.0}));
            for (double x : xs) {
                System.out.println(_p(x) + " " + _p(lanczos7(x)));
            }
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
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
