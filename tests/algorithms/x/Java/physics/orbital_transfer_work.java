public class Main {

    static double pow10(long n) {
        double p = (double)(1.0);
        if ((long)(n) >= 0L) {
            long i_2 = 0L;
            while ((long)(i_2) < (long)(n)) {
                p = (double)((double)(p) * (double)(10.0));
                i_2 = (long)((long)(i_2) + 1L);
            }
        } else {
            long i_3 = 0L;
            while ((long)(i_3) > (long)(n)) {
                p = (double)((double)(p) / (double)(10.0));
                i_3 = (long)((long)(i_3) - 1L);
            }
        }
        return p;
    }

    static double floor(double x) {
        long i_4 = (long)(((Number)(x)).intValue());
        double f_1 = (double)(((Number)(i_4)).doubleValue());
        if ((double)(f_1) > (double)(x)) {
            return ((Number)(((long)(i_4) - 1L))).doubleValue();
        }
        return f_1;
    }

    static String format_scientific_3(double x) {
        if ((double)(x) == (double)(0.0)) {
            return "0.000e+00";
        }
        String sign_1 = "";
        double num_1 = (double)(x);
        if ((double)(num_1) < (double)(0.0)) {
            sign_1 = "-";
            num_1 = (double)(-num_1);
        }
        long exp_1 = 0L;
        while ((double)(num_1) >= (double)(10.0)) {
            num_1 = (double)((double)(num_1) / (double)(10.0));
            exp_1 = (long)((long)(exp_1) + 1L);
        }
        while ((double)(num_1) < (double)(1.0)) {
            num_1 = (double)((double)(num_1) * (double)(10.0));
            exp_1 = (long)((long)(exp_1) - 1L);
        }
        double temp_1 = ((Number)(Math.floor((double)((double)(num_1) * (double)(1000.0)) + (double)(0.5)))).doubleValue();
        long scaled_1 = (long)(((Number)(temp_1)).intValue());
        if ((long)(scaled_1) == 10000L) {
            scaled_1 = 1000L;
            exp_1 = (long)((long)(exp_1) + 1L);
        }
        long int_part_1 = (long)((long)(scaled_1) / 1000L);
        long frac_part_1 = Math.floorMod(scaled_1, 1000);
        String frac_str_1 = _p(frac_part_1);
        while ((long)(_runeLen(frac_str_1)) < 3L) {
            frac_str_1 = "0" + frac_str_1;
        }
        String mantissa_1 = _p(int_part_1) + "." + frac_str_1;
        String exp_sign_1 = "+";
        long exp_abs_1 = (long)(exp_1);
        if ((long)(exp_1) < 0L) {
            exp_sign_1 = "-";
            exp_abs_1 = (long)(-exp_1);
        }
        String exp_str_1 = _p(exp_abs_1);
        if ((long)(exp_abs_1) < 10L) {
            exp_str_1 = "0" + exp_str_1;
        }
        return sign_1 + mantissa_1 + "e" + exp_sign_1 + exp_str_1;
    }

    static String orbital_transfer_work(double mass_central, double mass_object, double r_initial, double r_final) {
        double G = (double)((double)(6.6743) * (double)(pow10((long)(-11))));
        if ((double)(r_initial) <= (double)(0.0) || (double)(r_final) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Orbital radii must be greater than zero."));
        }
        double work_1 = (double)((double)(((double)((double)((double)(G) * (double)(mass_central)) * (double)(mass_object)) / (double)(2.0))) * (double)(((double)((double)(1.0) / (double)(r_initial)) - (double)((double)(1.0) / (double)(r_final)))));
        return format_scientific_3((double)(work_1));
    }

    static void test_orbital_transfer_work() {
        if (!(orbital_transfer_work((double)((double)(5.972) * (double)(pow10(24L))), (double)(1000.0), (double)((double)(6.371) * (double)(pow10(6L))), (double)((double)(7.0) * (double)(pow10(6L)))).equals("2.811e+09"))) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (!(orbital_transfer_work((double)((double)(5.972) * (double)(pow10(24L))), (double)(500.0), (double)((double)(7.0) * (double)(pow10(6L))), (double)((double)(6.371) * (double)(pow10(6L)))).equals("-1.405e+09"))) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (!(orbital_transfer_work((double)((double)(1.989) * (double)(pow10(30L))), (double)(1000.0), (double)((double)(1.5) * (double)(pow10(11L))), (double)((double)(2.28) * (double)(pow10(11L)))).equals("1.514e+11"))) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
    }

    static void main() {
        test_orbital_transfer_work();
        System.out.println(orbital_transfer_work((double)((double)(5.972) * (double)(pow10(24L))), (double)(1000.0), (double)((double)(6.371) * (double)(pow10(6L))), (double)((double)(7.0) * (double)(pow10(6L)))));
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
