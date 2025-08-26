public class Main {

    static double to_float(java.math.BigInteger x) {
        return (double)(((java.math.BigInteger)(x)).doubleValue() * (double)(1.0));
    }

    static double ln(double x) {
        if ((double)(x) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y_1 = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double y2_1 = (double)((double)(y_1) * (double)(y_1));
        double term_1 = (double)(y_1);
        double sum_1 = (double)(0.0);
        java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
        while (k_1.compareTo(java.math.BigInteger.valueOf(10)) < 0) {
            double denom_1 = (double)(((Number)(java.math.BigInteger.valueOf(2).multiply(k_1).add(java.math.BigInteger.valueOf(1)))).doubleValue());
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)(denom_1)));
            term_1 = (double)((double)(term_1) * (double)(y2_1));
            k_1 = k_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)((double)(2.0) * (double)(sum_1));
    }

    static double exp(double x) {
        double term_2 = (double)(1.0);
        double sum_3 = (double)(1.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            term_2 = (double)((double)((double)(term_2) * (double)(x)) / (double)(((Number)(n_1)).doubleValue()));
            sum_3 = (double)((double)(sum_3) + (double)(term_2));
            n_1 = n_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(sum_3);
    }

    static double pow_float(double base, double exponent) {
        return (double)(Math.exp((double)(exponent) * (double)(Math.log(base))));
    }

    static double get_altitude_at_pressure(double pressure) {
        if ((double)(pressure) > (double)(101325.0)) {
            throw new RuntimeException(String.valueOf("Value Higher than Pressure at Sea Level !"));
        }
        if ((double)(pressure) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Atmospheric Pressure can not be negative !"));
        }
        double ratio_1 = (double)((double)(pressure) / (double)(101325.0));
        return (double)((double)(44330.0) * (double)(((double)(1.0) - (double)(pow_float((double)(ratio_1), (double)((double)(1.0) / (double)(5.5255)))))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(get_altitude_at_pressure((double)(100000.0))));
            System.out.println(_p(get_altitude_at_pressure((double)(101325.0))));
            System.out.println(_p(get_altitude_at_pressure((double)(80000.0))));
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
