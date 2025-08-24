public class Main {

    static java.math.BigInteger[] iterator_values(java.math.BigInteger[][] matrix) {
        java.math.BigInteger[] result = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        for (java.math.BigInteger[] row : matrix) {
            for (java.math.BigInteger value : row) {
                result = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(value)).toArray(java.math.BigInteger[]::new)));
            }
        }
        return result;
    }

    static java.math.BigInteger index_2d_array_in_1d(java.math.BigInteger[][] array, java.math.BigInteger index) {
        java.math.BigInteger rows = new java.math.BigInteger(String.valueOf(array.length));
        java.math.BigInteger cols_1 = new java.math.BigInteger(String.valueOf(array[(int)(0L)].length));
        if (rows.compareTo(java.math.BigInteger.valueOf(0)) == 0 || cols_1.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("no items in array"));
        }
        if (index.compareTo(java.math.BigInteger.valueOf(0)) < 0 || index.compareTo(rows.multiply(cols_1)) >= 0) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        return array[(int)((long)(((Number)(index.divide(cols_1))).intValue()))][(int)(((java.math.BigInteger)(index.remainder(cols_1))).longValue())];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(iterator_values(((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(523)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(34)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0)}))})))));
            System.out.println(_p(iterator_values(((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(523)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(34), java.math.BigInteger.valueOf(0)}))})))));
            System.out.println(_p(index_2d_array_in_1d(((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(11)}))})), java.math.BigInteger.valueOf(5))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
