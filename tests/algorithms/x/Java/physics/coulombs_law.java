public class Main {
    static double K = (double)(8.9875517923e+09);

    static String format2(double x) {
        String sign = String.valueOf((double)(x) < (double)(0.0) ? "-" : "");
        double y_1 = (double)((double)(x) < (double)(0.0) ? -x : x);
        double m_1 = (double)(100.0);
        double scaled_1 = (double)((double)(y_1) * (double)(m_1));
        java.math.BigInteger i_1 = new java.math.BigInteger(String.valueOf(((Number)(scaled_1)).intValue()));
        if ((double)((double)(scaled_1) - (double)((((Number)(i_1)).doubleValue()))) >= (double)(0.5)) {
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger int_part_1 = new java.math.BigInteger(String.valueOf(i_1.divide(java.math.BigInteger.valueOf(100))));
        java.math.BigInteger frac_part_1 = new java.math.BigInteger(String.valueOf(i_1.remainder(java.math.BigInteger.valueOf(100))));
        String frac_str_1 = _p(frac_part_1);
        if (frac_part_1.compareTo(java.math.BigInteger.valueOf(10)) < 0) {
            frac_str_1 = "0" + frac_str_1;
        }
        return sign + _p(int_part_1) + "." + frac_str_1;
    }

    static double coulombs_law(double q1, double q2, double radius) {
        if ((double)(radius) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("radius must be positive"));
        }
        double force_1 = (double)((double)((double)((double)(K) * (double)(q1)) * (double)(q2)) / (double)(((double)(radius) * (double)(radius))));
        return (double)(force_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(format2((double)(coulombs_law((double)(15.5), (double)(20.0), (double)(15.0)))));
            System.out.println(format2((double)(coulombs_law((double)(1.0), (double)(15.0), (double)(5.0)))));
            System.out.println(format2((double)(coulombs_law((double)(20.0), (double)(-50.0), (double)(15.0)))));
            System.out.println(format2((double)(coulombs_law((double)(-5.0), (double)(-8.0), (double)(10.0)))));
            System.out.println(format2((double)(coulombs_law((double)(50.0), (double)(100.0), (double)(50.0)))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
