public class Main {

    static long pow2_int(long n) {
        long result = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            result = (long)((long)(result) * 2L);
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static double pow2_float(long n) {
        double result_1 = (double)(1.0);
        if ((long)(n) >= 0L) {
            long i_4 = 0L;
            while ((long)(i_4) < (long)(n)) {
                result_1 = (double)((double)(result_1) * (double)(2.0));
                i_4 = (long)((long)(i_4) + 1L);
            }
        } else {
            long i_5 = 0L;
            long m_1 = (long)(0L - (long)(n));
            while ((long)(i_5) < (long)(m_1)) {
                result_1 = (double)((double)(result_1) / (double)(2.0));
                i_5 = (long)((long)(i_5) + 1L);
            }
        }
        return result_1;
    }

    static long lshift(long num, long k) {
        long result_2 = (long)(num);
        long i_7 = 0L;
        while ((long)(i_7) < (long)(k)) {
            result_2 = (long)((long)(result_2) * 2L);
            i_7 = (long)((long)(i_7) + 1L);
        }
        return result_2;
    }

    static long rshift(long num, long k) {
        long result_3 = (long)(num);
        long i_9 = 0L;
        while ((long)(i_9) < (long)(k)) {
            result_3 = Math.floorDiv(((long)(result_3) - (long)((Math.floorMod(result_3, 2)))), 2);
            i_9 = (long)((long)(i_9) + 1L);
        }
        return result_3;
    }

    static long log2_floor(double x) {
        double n = (double)(x);
        long e_1 = 0L;
        while ((double)(n) >= (double)(2.0)) {
            n = (double)((double)(n) / (double)(2.0));
            e_1 = (long)((long)(e_1) + 1L);
        }
        while ((double)(n) < (double)(1.0)) {
            n = (double)((double)(n) * (double)(2.0));
            e_1 = (long)((long)(e_1) - 1L);
        }
        return e_1;
    }

    static long float_to_bits(double x) {
        double num = (double)(x);
        long sign_1 = 0L;
        if ((double)(num) < (double)(0.0)) {
            sign_1 = 1L;
            num = (double)(-num);
        }
        long exp_1 = (long)(log2_floor((double)(num)));
        double pow_1 = (double)(pow2_float((long)(exp_1)));
        double normalized_1 = (double)((double)(num) / (double)(pow_1));
        double frac_1 = (double)((double)(normalized_1) - (double)(1.0));
        long mantissa_1 = (long)(((Number)(((double)(frac_1) * (double)(pow2_float(23L))))).intValue());
        long exp_bits_1 = (long)((long)(exp_1) + 127L);
        return (long)((long)(lshift((long)(sign_1), 31L)) + (long)(lshift((long)(exp_bits_1), 23L))) + (long)(mantissa_1);
    }

    static double bits_to_float(long bits) {
        long sign_bit = Math.floorMod(rshift((long)(bits), 31L), 2);
        double sign_3 = (double)(1.0);
        if ((long)(sign_bit) == 1L) {
            sign_3 = (double)(-1.0);
        }
        long exp_bits_3 = Math.floorMod(rshift((long)(bits), 23L), 256);
        long exp_3 = (long)((long)(exp_bits_3) - 127L);
        long mantissa_bits_1 = Math.floorMod(bits, pow2_int(23L));
        double mantissa_3 = (double)((double)(1.0) + (double)((double)((((Number)(mantissa_bits_1)).doubleValue())) / (double)(pow2_float(23L))));
        return (double)((double)(sign_3) * (double)(mantissa_3)) * (double)(pow2_float((long)(exp_3)));
    }

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        long i_11 = 0L;
        while ((long)(i_11) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return guess_1;
    }

    static boolean is_close(double a, double b, double rel_tol) {
        return (double)(absf((double)((double)(a) - (double)(b)))) <= (double)((double)(rel_tol) * (double)(absf((double)(b))));
    }

    static double fast_inverse_sqrt(double number) {
        if ((double)(number) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Input must be a positive number."));
        }
        long i_13 = (long)(float_to_bits((double)(number)));
        long magic_1 = 1597463007L;
        long y_bits_1 = (long)((long)(magic_1) - (long)(rshift((long)(i_13), 1L)));
        double y_1 = (double)(bits_to_float((long)(y_bits_1)));
        y_1 = (double)((double)(y_1) * (double)(((double)(1.5) - (double)((double)((double)((double)(0.5) * (double)(number)) * (double)(y_1)) * (double)(y_1)))));
        return y_1;
    }

    static void test_fast_inverse_sqrt() {
        if ((double)(absf((double)((double)(fast_inverse_sqrt((double)(10.0))) - (double)(0.3156857923527257)))) > (double)(0.0001)) {
            throw new RuntimeException(String.valueOf("fast_inverse_sqrt(10) failed"));
        }
        if ((double)(absf((double)((double)(fast_inverse_sqrt((double)(4.0))) - (double)(0.49915357479239103)))) > (double)(0.0001)) {
            throw new RuntimeException(String.valueOf("fast_inverse_sqrt(4) failed"));
        }
        if ((double)(absf((double)((double)(fast_inverse_sqrt((double)(4.1))) - (double)(0.4932849504615651)))) > (double)(0.0001)) {
            throw new RuntimeException(String.valueOf("fast_inverse_sqrt(4.1) failed"));
        }
        long i_15 = 50L;
        while ((long)(i_15) < 60L) {
            double y_3 = (double)(fast_inverse_sqrt((double)(((Number)(i_15)).doubleValue())));
            double actual_1 = (double)((double)(1.0) / (double)(sqrtApprox((double)(((Number)(i_15)).doubleValue()))));
            if (!(Boolean)is_close((double)(y_3), (double)(actual_1), (double)(0.00132))) {
                throw new RuntimeException(String.valueOf("relative error too high"));
            }
            i_15 = (long)((long)(i_15) + 1L);
        }
    }

    static void main() {
        test_fast_inverse_sqrt();
        long i_17 = 5L;
        while ((long)(i_17) <= 100L) {
            double diff_1 = (double)((double)(((double)(1.0) / (double)(sqrtApprox((double)(((Number)(i_17)).doubleValue()))))) - (double)(fast_inverse_sqrt((double)(((Number)(i_17)).doubleValue()))));
            System.out.println(_p(i_17) + ": " + _p(diff_1));
            i_17 = (long)((long)(i_17) + 5L);
        }
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
