public class Main {
    static class PrefixSum {
        java.math.BigInteger[] prefix_sum;
        PrefixSum(java.math.BigInteger[] prefix_sum) {
            this.prefix_sum = prefix_sum;
        }
        PrefixSum() {}
        @Override public String toString() {
            return String.format("{'prefix_sum': %s}", String.valueOf(prefix_sum));
        }
    }

    static PrefixSum ps;
    static PrefixSum ps2;

    static PrefixSum make_prefix_sum(java.math.BigInteger[] arr) {
        java.math.BigInteger[] prefix = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger running_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            running_1 = new java.math.BigInteger(String.valueOf(running_1.add(arr[(int)(((java.math.BigInteger)(i_1)).longValue())])));
            prefix = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(prefix), java.util.stream.Stream.of(running_1)).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new PrefixSum(prefix);
    }

    static java.math.BigInteger get_sum(PrefixSum ps, java.math.BigInteger start, java.math.BigInteger end) {
        java.math.BigInteger[] prefix_1 = ((java.math.BigInteger[])(ps.prefix_sum));
        if (new java.math.BigInteger(String.valueOf(prefix_1.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("The array is empty."));
        }
        if (start.compareTo(java.math.BigInteger.valueOf(0)) < 0 || end.compareTo(new java.math.BigInteger(String.valueOf(prefix_1.length))) >= 0 || start.compareTo(end) > 0) {
            throw new RuntimeException(String.valueOf("Invalid range specified."));
        }
        if (start.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return prefix_1[(int)(((java.math.BigInteger)(end)).longValue())];
        }
        return prefix_1[(int)(((java.math.BigInteger)(end)).longValue())].subtract(prefix_1[(int)(((java.math.BigInteger)(start.subtract(java.math.BigInteger.valueOf(1)))).longValue())]);
    }

    static boolean contains_sum(PrefixSum ps, java.math.BigInteger target_sum) {
        java.math.BigInteger[] prefix_2 = ((java.math.BigInteger[])(ps.prefix_sum));
        java.math.BigInteger[] sums_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0)}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(prefix_2.length))) < 0) {
            java.math.BigInteger sum_item_1 = new java.math.BigInteger(String.valueOf(prefix_2[(int)(((java.math.BigInteger)(i_3)).longValue())]));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(sums_1.length))) < 0) {
                if (sums_1[(int)(((java.math.BigInteger)(j_1)).longValue())].compareTo(sum_item_1.subtract(target_sum)) == 0) {
                    return true;
                }
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            sums_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(sums_1), java.util.stream.Stream.of(sum_item_1)).toArray(java.math.BigInteger[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return false;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ps = make_prefix_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})));
            System.out.println(_p(get_sum(ps, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2))));
            System.out.println(_p(get_sum(ps, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2))));
            System.out.println(_p(get_sum(ps, java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(2))));
            System.out.println(_p(contains_sum(ps, java.math.BigInteger.valueOf(6))));
            System.out.println(_p(contains_sum(ps, java.math.BigInteger.valueOf(5))));
            System.out.println(_p(contains_sum(ps, java.math.BigInteger.valueOf(3))));
            System.out.println(_p(contains_sum(ps, java.math.BigInteger.valueOf(4))));
            System.out.println(_p(contains_sum(ps, java.math.BigInteger.valueOf(7))));
            ps2 = make_prefix_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), java.math.BigInteger.valueOf(3)})));
            System.out.println(_p(contains_sum(ps2, java.math.BigInteger.valueOf(2))));
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
