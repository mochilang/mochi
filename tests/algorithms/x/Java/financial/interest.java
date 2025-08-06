public class Main {

    static void panic(String msg) {
        System.out.println(msg);
    }

    static double powf(double base, double exp) {
        double result = 1.0;
        int i = 0;
        while (i < ((Number)(exp)).intValue()) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double simple_interest(double principal, double daily_rate, double days) {
        if (days <= 0.0) {
            throw new RuntimeException(String.valueOf("days_between_payments must be > 0"));
            return 0.0;
        }
        if (daily_rate < 0.0) {
            throw new RuntimeException(String.valueOf("daily_interest_rate must be >= 0"));
            return 0.0;
        }
        if (principal <= 0.0) {
            throw new RuntimeException(String.valueOf("principal must be > 0"));
            return 0.0;
        }
        return principal * daily_rate * days;
    }

    static double compound_interest(double principal, double nominal_rate, double periods) {
        if (periods <= 0.0) {
            throw new RuntimeException(String.valueOf("number_of_compounding_periods must be > 0"));
            return 0.0;
        }
        if (nominal_rate < 0.0) {
            throw new RuntimeException(String.valueOf("nominal_annual_interest_rate_percentage must be >= 0"));
            return 0.0;
        }
        if (principal <= 0.0) {
            throw new RuntimeException(String.valueOf("principal must be > 0"));
            return 0.0;
        }
        return principal * (powf(1.0 + nominal_rate, periods) - 1.0);
    }

    static double apr_interest(double principal, double apr, double years) {
        if (years <= 0.0) {
            throw new RuntimeException(String.valueOf("number_of_years must be > 0"));
            return 0.0;
        }
        if (apr < 0.0) {
            throw new RuntimeException(String.valueOf("nominal_annual_percentage_rate must be >= 0"));
            return 0.0;
        }
        if (principal <= 0.0) {
            throw new RuntimeException(String.valueOf("principal must be > 0"));
            return 0.0;
        }
        return compound_interest(principal, apr / 365.0, years * 365.0);
    }

    static void main() {
        System.out.println(_p(simple_interest(18000.0, 0.06, 3.0)));
        System.out.println(_p(simple_interest(0.5, 0.06, 3.0)));
        System.out.println(_p(simple_interest(18000.0, 0.01, 10.0)));
        System.out.println(_p(compound_interest(10000.0, 0.05, 3.0)));
        System.out.println(_p(compound_interest(10000.0, 0.05, 1.0)));
        System.out.println(_p(apr_interest(10000.0, 0.05, 3.0)));
        System.out.println(_p(apr_interest(10000.0, 0.05, 1.0)));
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
