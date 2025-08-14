public class Main {

    static void panic(String msg) {
        System.out.println(msg);
    }

    static double powf(double base, double exp) {
        double result = (double)(1.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(((Number)(exp)).intValue())) {
            result = (double)((double)(result) * (double)(base));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static double simple_interest(double principal, double daily_rate, double days) {
        if ((double)(days) <= (double)(0.0)) {
            panic("days_between_payments must be > 0");
        }
        if ((double)(daily_rate) < (double)(0.0)) {
            panic("daily_interest_rate must be >= 0");
        }
        if ((double)(principal) <= (double)(0.0)) {
            panic("principal must be > 0");
        }
        return (double)((double)(principal) * (double)(daily_rate)) * (double)(days);
    }

    static double compound_interest(double principal, double nominal_rate, double periods) {
        if ((double)(periods) <= (double)(0.0)) {
            panic("number_of_compounding_periods must be > 0");
        }
        if ((double)(nominal_rate) < (double)(0.0)) {
            panic("nominal_annual_interest_rate_percentage must be >= 0");
        }
        if ((double)(principal) <= (double)(0.0)) {
            panic("principal must be > 0");
        }
        return (double)(principal) * (double)(((double)(powf((double)((double)(1.0) + (double)(nominal_rate)), (double)(periods))) - (double)(1.0)));
    }

    static double apr_interest(double principal, double apr, double years) {
        if ((double)(years) <= (double)(0.0)) {
            panic("number_of_years must be > 0");
        }
        if ((double)(apr) < (double)(0.0)) {
            panic("nominal_annual_percentage_rate must be >= 0");
        }
        if ((double)(principal) <= (double)(0.0)) {
            panic("principal must be > 0");
        }
        return compound_interest((double)(principal), (double)((double)(apr) / (double)(365.0)), (double)((double)(years) * (double)(365.0)));
    }

    static void main() {
        System.out.println(_p(simple_interest((double)(18000.0), (double)(0.06), (double)(3.0))));
        System.out.println(_p(simple_interest((double)(0.5), (double)(0.06), (double)(3.0))));
        System.out.println(_p(simple_interest((double)(18000.0), (double)(0.01), (double)(10.0))));
        System.out.println(_p(compound_interest((double)(10000.0), (double)(0.05), (double)(3.0))));
        System.out.println(_p(compound_interest((double)(10000.0), (double)(0.05), (double)(1.0))));
        System.out.println(_p(apr_interest((double)(10000.0), (double)(0.05), (double)(3.0))));
        System.out.println(_p(apr_interest((double)(10000.0), (double)(0.05), (double)(1.0))));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
