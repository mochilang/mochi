public class Main {
    static double PI;

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow(double x, int n) {
        double result = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            result = result * x;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double factorial(int n) {
        double result_1 = 1.0;
        int i_2 = 2;
        while (i_2 <= n) {
            result_1 = result_1 * (((Number)(i_2)).doubleValue());
            i_2 = i_2 + 1;
        }
        return result_1;
    }

    static double radians(double deg) {
        return deg * PI / 180.0;
    }

    static double taylor_sin(double angle_in_degrees, int accuracy, int rounded_values_count) {
        double k = floor(angle_in_degrees / 360.0);
        double angle = angle_in_degrees - (k * 360.0);
        double angle_in_radians = radians(angle);
        double result_2 = angle_in_radians;
        int a = 3;
        double sign = -1.0;
        int i_3 = 0;
        while (i_3 < accuracy) {
            result_2 = result_2 + (sign * pow(angle_in_radians, a)) / factorial(a);
            sign = -sign;
            a = a + 2;
            i_3 = i_3 + 1;
        }
        return result_2;
    }

    static void test_sin() {
        double eps = 1e-07;
        if (Math.abs(taylor_sin(0.0, 18, 10) - 0.0) > eps) {
            throw new RuntimeException(String.valueOf("sin(0) failed"));
        }
        if (Math.abs(taylor_sin(90.0, 18, 10) - 1.0) > eps) {
            throw new RuntimeException(String.valueOf("sin(90) failed"));
        }
        if (Math.abs(taylor_sin(180.0, 18, 10) - 0.0) > eps) {
            throw new RuntimeException(String.valueOf("sin(180) failed"));
        }
        if (Math.abs(taylor_sin(270.0, 18, 10) - (-1.0)) > eps) {
            throw new RuntimeException(String.valueOf("sin(270) failed"));
        }
    }

    static void main() {
        test_sin();
        double res = taylor_sin(64.0, 18, 10);
        System.out.println(res);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
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
