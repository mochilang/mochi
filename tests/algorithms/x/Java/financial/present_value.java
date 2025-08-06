public class Main {

    static double powf(double base, int exponent) {
        double result = 1.0;
        int i = 0;
        while (i < exponent) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double round2(double value) {
        if (value >= 0.0) {
            int scaled = ((Number)((value * 100.0 + 0.5))).intValue();
            return (((Number)(scaled)).doubleValue()) / 100.0;
        }
        int scaled_1 = ((Number)((value * 100.0 - 0.5))).intValue();
        return (((Number)(scaled_1)).doubleValue()) / 100.0;
    }

    static double present_value(double discount_rate, double[] cash_flows) {
        if (discount_rate < 0.0) {
            throw new RuntimeException(String.valueOf("Discount rate cannot be negative"));
        }
        if (cash_flows.length == 0) {
            throw new RuntimeException(String.valueOf("Cash flows list cannot be empty"));
        }
        double pv = 0.0;
        int i_1 = 0;
        double factor = 1.0 + discount_rate;
        while (i_1 < cash_flows.length) {
            double cf = cash_flows[i_1];
            pv = pv + cf / powf(factor, i_1);
            i_1 = i_1 + 1;
        }
        return round2(pv);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(present_value(0.13, ((double[])(new double[]{10.0, 20.7, -293.0, 297.0})))));
            System.out.println(_p(present_value(0.07, ((double[])(new double[]{-109129.39, 30923.23, 15098.93, 29734.0, 39.0})))));
            System.out.println(_p(present_value(0.07, ((double[])(new double[]{109129.39, 30923.23, 15098.93, 29734.0, 39.0})))));
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
