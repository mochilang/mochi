public class Main {
    static java.math.BigInteger seed = java.math.BigInteger.valueOf(1);
    static java.math.BigInteger[] integers = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7)}));
    static String[] strings = ((String[])(new String[]{"python", "says", "hello", "!"}));

    static java.math.BigInteger rand() {
        seed = new java.math.BigInteger(String.valueOf((seed.multiply(java.math.BigInteger.valueOf(1103515245)).add(java.math.BigInteger.valueOf(12345))).remainder(java.math.BigInteger.valueOf(2147483648L))));
        return new java.math.BigInteger(String.valueOf(seed.divide(java.math.BigInteger.valueOf(65536))));
    }

    static java.math.BigInteger randint(java.math.BigInteger a, java.math.BigInteger b) {
        java.math.BigInteger r = new java.math.BigInteger(String.valueOf(rand()));
        return new java.math.BigInteger(String.valueOf(a.add(r.remainder((b.subtract(a).add(java.math.BigInteger.valueOf(1)))))));
    }

    static java.math.BigInteger[] fisher_yates_shuffle_int(java.math.BigInteger[] data) {
        java.math.BigInteger[] res = ((java.math.BigInteger[])(data));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(res.length))) < 0) {
            java.math.BigInteger a_1 = new java.math.BigInteger(String.valueOf(randint(java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(res.length)).subtract(java.math.BigInteger.valueOf(1)))))));
            java.math.BigInteger b_1 = new java.math.BigInteger(String.valueOf(randint(java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(res.length)).subtract(java.math.BigInteger.valueOf(1)))))));
            java.math.BigInteger temp_1 = new java.math.BigInteger(String.valueOf(res[_idx((res).length, ((java.math.BigInteger)(a_1)).longValue())]));
res[(int)(((java.math.BigInteger)(a_1)).longValue())] = new java.math.BigInteger(String.valueOf(res[_idx((res).length, ((java.math.BigInteger)(b_1)).longValue())]));
res[(int)(((java.math.BigInteger)(b_1)).longValue())] = new java.math.BigInteger(String.valueOf(temp_1));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(res));
    }

    static String[] fisher_yates_shuffle_str(String[] data) {
        String[] res_1 = ((String[])(data));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(res_1.length))) < 0) {
            java.math.BigInteger a_3 = new java.math.BigInteger(String.valueOf(randint(java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(res_1.length)).subtract(java.math.BigInteger.valueOf(1)))))));
            java.math.BigInteger b_3 = new java.math.BigInteger(String.valueOf(randint(java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(res_1.length)).subtract(java.math.BigInteger.valueOf(1)))))));
            String temp_3 = res_1[_idx((res_1).length, ((java.math.BigInteger)(a_3)).longValue())];
res_1[(int)(((java.math.BigInteger)(a_3)).longValue())] = res_1[_idx((res_1).length, ((java.math.BigInteger)(b_3)).longValue())];
res_1[(int)(((java.math.BigInteger)(b_3)).longValue())] = temp_3;
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((String[])(res_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("Fisher-Yates Shuffle:");
            System.out.println("List " + _p(integers) + " " + _p(strings));
            System.out.println("FY Shuffle " + _p(fisher_yates_shuffle_int(((java.math.BigInteger[])(integers)))) + " " + _p(fisher_yates_shuffle_str(((String[])(strings)))));
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
