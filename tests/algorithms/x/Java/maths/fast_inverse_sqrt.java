public class Main {

    static long pow2_int(long n) {
        long result = 1;
        long i_1 = 0;
        while (i_1 < n) {
            result = result * 2;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double pow2_float(long n) {
        double result_1 = 1.0;
        if (n >= 0) {
            long i_4 = 0;
            while (i_4 < n) {
                result_1 = result_1 * 2.0;
                i_4 = i_4 + 1;
            }
        } else {
            long i_5 = 0;
            long m_1 = 0 - n;
            while (i_5 < m_1) {
                result_1 = result_1 / 2.0;
                i_5 = i_5 + 1;
            }
        }
        return result_1;
    }

    static long lshift(long num, long k) {
        long result_2 = num;
        long i_7 = 0;
        while (i_7 < k) {
            result_2 = result_2 * 2;
            i_7 = i_7 + 1;
        }
        return result_2;
    }

    static long rshift(long num, long k) {
        long result_3 = num;
        long i_9 = 0;
        while (i_9 < k) {
            result_3 = Math.floorDiv((result_3 - (Math.floorMod(result_3, 2))), 2);
            i_9 = i_9 + 1;
        }
        return result_3;
    }

    static long log2_floor(double x) {
        double n = x;
        long e_1 = 0;
        while (n >= 2.0) {
            n = n / 2.0;
            e_1 = e_1 + 1;
        }
        while (n < 1.0) {
            n = n * 2.0;
            e_1 = e_1 - 1;
        }
        return e_1;
    }

    static long float_to_bits(double x) {
        double num = x;
        long sign_1 = 0;
        if (num < 0.0) {
            sign_1 = 1;
            num = -num;
        }
        long exp_1 = log2_floor(num);
        double pow_1 = pow2_float(exp_1);
        double normalized_1 = num / pow_1;
        double frac_1 = normalized_1 - 1.0;
        long mantissa_1 = ((Number)((frac_1 * pow2_float(23)))).intValue();
        long exp_bits_1 = exp_1 + 127;
        return lshift(sign_1, 31) + lshift(exp_bits_1, 23) + mantissa_1;
    }

    static double bits_to_float(long bits) {
        long sign_bit = Math.floorMod(rshift(bits, 31), 2);
        double sign_3 = 1.0;
        if (sign_bit == 1) {
            sign_3 = -1.0;
        }
        long exp_bits_3 = Math.floorMod(rshift(bits, 23), 256);
        long exp_3 = exp_bits_3 - 127;
        long mantissa_bits_1 = Math.floorMod(bits, pow2_int(23));
        double mantissa_3 = 1.0 + (((Number)(mantissa_bits_1)).doubleValue()) / pow2_float(23);
        return sign_3 * mantissa_3 * pow2_float(exp_3);
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
        double guess_1 = x / 2.0;
        long i_11 = 0;
        while (i_11 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_11 = i_11 + 1;
        }
        return guess_1;
    }

    static boolean is_close(double a, double b, double rel_tol) {
        return absf(a - b) <= rel_tol * absf(b);
    }

    static double fast_inverse_sqrt(double number) {
        if (number <= 0.0) {
            throw new RuntimeException(String.valueOf("Input must be a positive number."));
        }
        long i_13 = float_to_bits(number);
        long magic_1 = 1597463007;
        long y_bits_1 = magic_1 - rshift(i_13, 1);
        double y_1 = bits_to_float(y_bits_1);
        y_1 = y_1 * (1.5 - 0.5 * number * y_1 * y_1);
        return y_1;
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
        long i_15 = 50;
        while (i_15 < 60) {
            double y_3 = fast_inverse_sqrt(((Number)(i_15)).doubleValue());
            double actual_1 = 1.0 / sqrtApprox(((Number)(i_15)).doubleValue());
            if (!(Boolean)is_close(y_3, actual_1, 0.00132)) {
                throw new RuntimeException(String.valueOf("relative error too high"));
            }
            i_15 = i_15 + 1;
        }
    }

    static void main() {
        test_fast_inverse_sqrt();
        long i_17 = 5;
        while (i_17 <= 100) {
            double diff_1 = (1.0 / sqrtApprox(((Number)(i_17)).doubleValue())) - fast_inverse_sqrt(((Number)(i_17)).doubleValue());
            System.out.println(_p(i_17) + ": " + _p(diff_1));
            i_17 = i_17 + 5;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
