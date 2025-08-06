public class Main {

    static double[] straight_line_depreciation(int useful_years, double purchase_value, double residual_value) {
        if (useful_years < 1) {
            throw new RuntimeException(String.valueOf("Useful years cannot be less than 1"));
        }
        if (purchase_value < 0.0) {
            throw new RuntimeException(String.valueOf("Purchase value cannot be less than zero"));
        }
        if (purchase_value < residual_value) {
            throw new RuntimeException(String.valueOf("Purchase value cannot be less than residual value"));
        }
        double depreciable_cost = purchase_value - residual_value;
        double annual_expense = depreciable_cost / (1.0 * useful_years);
        double[] expenses = ((double[])(new double[]{}));
        double accumulated = 0.0;
        int period = 0;
        while (period < useful_years) {
            if (period != useful_years - 1) {
                accumulated = accumulated + annual_expense;
                expenses = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(expenses), java.util.stream.DoubleStream.of(annual_expense)).toArray()));
            } else {
                double end_year_expense = depreciable_cost - accumulated;
                expenses = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(expenses), java.util.stream.DoubleStream.of(end_year_expense)).toArray()));
            }
            period = period + 1;
        }
        return expenses;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(straight_line_depreciation(10, 1100.0, 100.0)));
            System.out.println(_p(straight_line_depreciation(6, 1250.0, 50.0)));
            System.out.println(_p(straight_line_depreciation(4, 1001.0, 0.0)));
            System.out.println(_p(straight_line_depreciation(11, 380.0, 50.0)));
            System.out.println(_p(straight_line_depreciation(1, 4985.0, 100.0)));
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
