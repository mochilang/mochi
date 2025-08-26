public class Main {
    static java.math.BigInteger[] start = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(5)}));
    static java.math.BigInteger[] finish = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(9)}));

    static void print_max_activities(java.math.BigInteger[] start, java.math.BigInteger[] finish) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(finish.length));
        System.out.println("The following activities are selected:");
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        String result_1 = "0,";
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
        while (j_1.compareTo(n) < 0) {
            if (start[_idx((start).length, ((java.math.BigInteger)(j_1)).longValue())].compareTo(finish[_idx((finish).length, ((java.math.BigInteger)(i_1)).longValue())]) >= 0) {
                result_1 = result_1 + _p(j_1) + ",";
                i_1 = j_1;
            }
            j_1 = j_1.add(java.math.BigInteger.valueOf(1));
        }
        System.out.println(result_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            print_max_activities(((java.math.BigInteger[])(start)), ((java.math.BigInteger[])(finish)));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
