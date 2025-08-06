public class Main {

    static int pow2_int(int n) {
        int result = 1;
        int i = 0;
        while (i < n) {
            result = result * 2;
            i = i + 1;
        }
        return result;
    }

    static double pow2_float(int n) {
        double result_1 = 1.0;
        if (n >= 0) {
            int i_1 = 0;
            while (i_1 < n) {
                result_1 = result_1 * 2.0;
                i_1 = i_1 + 1;
            }
        } else {
            int i_2 = 0;
            int m = 0 - n;
            while (i_2 < m) {
                result_1 = result_1 / 2.0;
                i_2 = i_2 + 1;
            }
        }
        return result_1;
    }

    static int lshift(int num, int k) {
        int result_2 = num;
        int i_3 = 0;
        while (i_3 < k) {
            result_2 = result_2 * 2;
            i_3 = i_3 + 1;
        }
        return result_2;
    }

    static int rshift(int num, int k) {
        int result_3 = num;
        int i_4 = 0;
        while (i_4 < k) {
            result_3 = (result_3 - (Math.floorMod(result_3, 2))) / 2;
            i_4 = i_4 + 1;
        }
        return result_3;
    }

    static int log2_floor(double x) {
        double n = x;
        int e = 0;
        while (n >= 2.0) {
            n = n / 2.0;
            e = e + 1;
        }
        while (n < 1.0) {
            n = n * 2.0;
            e = e - 1;
        }
        return e;
    }

    static int float_to_bits(double x) {
        double num = x;
        int sign = 0;
        if (num < 0.0) {
            sign = 1;
            num = -num;
        }
        int exp = log2_floor(num);
        double pow = pow2_float(exp);
        double normalized = num / pow;
        double frac = normalized - 1.0;
        int mantissa = ((Number)((frac * pow2_float(23)))).intValue();
        int exp_bits = exp + 127;
        return lshift(sign, 31) + lshift(exp_bits, 23) + mantissa;
    }

    static double bits_to_float(int bits) {
        int sign_bit = Math.floorMod(rshift(bits, 31), 2);
        double sign_1 = 1.0;
        if (sign_bit == 1) {
            sign_1 = -1.0;
        }
        int exp_bits_1 = Math.floorMod(rshift(bits, 23), 256);
        int exp_1 = exp_bits_1 - 127;
        int mantissa_bits = Math.floorMod(bits, pow2_int(23));
        double mantissa_1 = 1.0 + (((Number)(mantissa_bits)).doubleValue()) / pow2_float(23);
        return sign_1 * mantissa_1 * pow2_float(exp_1);
    }

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i_5 = 0;
        while (i_5 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_5 = i_5 + 1;
        }
        return guess;
    }

    static boolean is_close(double a, double b, double rel_tol) {
        return absf(a - b) <= rel_tol * absf(b);
    }

    static double fast_inverse_sqrt(double number) {
        if (number <= 0.0) {
            throw new RuntimeException(String.valueOf("Input must be a positive number."));
        }
        int i_6 = float_to_bits(number);
        int magic = 1597463007;
        int y_bits = magic - rshift(i_6, 1);
        double y = bits_to_float(y_bits);
        y = y * (1.5 - 0.5 * number * y * y);
        return y;
    }

    static void test_fast_inverse_sqrt() {
        if (absf(fast_inverse_sqrt(10.0) - 0.3156857923527257) > 0.0001) {
            throw new RuntimeException(String.valueOf("fast_inverse_sqrt(10) failed"));
        }
        if (absf(fast_inverse_sqrt(4.0) - 0.49915357479239103) > 0.0001) {
            throw new RuntimeException(String.valueOf("fast_inverse_sqrt(4) failed"));
        }
        if (absf(fast_inverse_sqrt(4.1) - 0.4932849504615651) > 0.0001) {
            throw new RuntimeException(String.valueOf("fast_inverse_sqrt(4.1) failed"));
        }
        int i_7 = 50;
        while (i_7 < 60) {
            double y_1 = fast_inverse_sqrt(((Number)(i_7)).doubleValue());
            double actual = 1.0 / sqrtApprox(((Number)(i_7)).doubleValue());
            if (!(Boolean)is_close(y_1, actual, 0.00132)) {
                throw new RuntimeException(String.valueOf("relative error too high"));
            }
            i_7 = i_7 + 1;
        }
    }

    static void main() {
        test_fast_inverse_sqrt();
        int i_8 = 5;
        while (i_8 <= 100) {
            double diff = (1.0 / sqrtApprox(((Number)(i_8)).doubleValue())) - fast_inverse_sqrt(((Number)(i_8)).doubleValue());
            System.out.println(_p(i_8) + ": " + _p(diff));
            i_8 = i_8 + 5;
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
