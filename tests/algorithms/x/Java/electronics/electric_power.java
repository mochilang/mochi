public class Main {
    static class Result {
        String name;
        double value;
        Result(String name, double value) {
            this.name = name;
            this.value = value;
        }
        Result() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'value': %s}", String.valueOf(name), String.valueOf(value));
        }
    }

    static Result r1;
    static Result r2;
    static Result r3;
    static Result r4;
    static Result r5;

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double pow10(int n) {
        double p = 1.0;
        int i = 0;
        while (i < n) {
            p = p * 10.0;
            i = i + 1;
        }
        return p;
    }

    static double round_to(double x, int n) {
        double m = pow10(n);
        return ((Number)(floor(x * m + 0.5))).intValue() / m;
    }

    static Result electric_power(double voltage, double current, double power) {
        int zeros = 0;
        if (voltage == 0.0) {
            zeros = zeros + 1;
        }
        if (current == 0.0) {
            zeros = zeros + 1;
        }
        if (power == 0.0) {
            zeros = zeros + 1;
        }
        if (zeros != 1) {
            throw new RuntimeException(String.valueOf("Exactly one argument must be 0"));
        } else         if (power < 0.0) {
            throw new RuntimeException(String.valueOf("Power cannot be negative in any electrical/electronics system"));
        } else         if (voltage == 0.0) {
            return new Result("voltage", power / current);
        } else         if (current == 0.0) {
            return new Result("current", power / voltage);
        } else         if (power == 0.0) {
            double p_1 = absf(voltage * current);
            return new Result("power", round_to(p_1, 2));
        } else {
            throw new RuntimeException(String.valueOf("Unhandled case"));
        }
    }

    static String str_result(Result r) {
        return "Result(name='" + r.name + "', value=" + _p(r.value) + ")";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = electric_power(0.0, 2.0, 5.0);
            System.out.println(str_result(r1));
            r2 = electric_power(2.0, 2.0, 0.0);
            System.out.println(str_result(r2));
            r3 = electric_power(-2.0, 3.0, 0.0);
            System.out.println(str_result(r3));
            r4 = electric_power(2.2, 2.2, 0.0);
            System.out.println(str_result(r4));
            r5 = electric_power(2.0, 0.0, 6.0);
            System.out.println(str_result(r5));
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
