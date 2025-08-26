public class Main {

    static java.math.BigInteger[] subarray(java.math.BigInteger[] xs, java.math.BigInteger start, java.math.BigInteger end) {
        java.math.BigInteger[] result = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger k_1 = start;
        while (k_1.compareTo(end) < 0) {
            result = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(xs[_idx((xs).length, ((java.math.BigInteger)(k_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            k_1 = k_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((java.math.BigInteger[])(result));
    }

    static java.math.BigInteger[] merge(java.math.BigInteger[] left_half, java.math.BigInteger[] right_half) {
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(left_half.length))) < 0 && j_1.compareTo(new java.math.BigInteger(String.valueOf(right_half.length))) < 0) {
            if (left_half[_idx((left_half).length, ((java.math.BigInteger)(i_1)).longValue())].compareTo(right_half[_idx((right_half).length, ((java.math.BigInteger)(j_1)).longValue())]) < 0) {
                result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(left_half[_idx((left_half).length, ((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
                i_1 = i_1.add(java.math.BigInteger.valueOf(1));
            } else {
                result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(right_half[_idx((right_half).length, ((java.math.BigInteger)(j_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
                j_1 = j_1.add(java.math.BigInteger.valueOf(1));
            }
        }
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(left_half.length))) < 0) {
            result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(left_half[_idx((left_half).length, ((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        while (j_1.compareTo(new java.math.BigInteger(String.valueOf(right_half.length))) < 0) {
            result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(right_half[_idx((right_half).length, ((java.math.BigInteger)(j_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            j_1 = j_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((java.math.BigInteger[])(result_1));
    }

    static java.math.BigInteger[] merge_sort(java.math.BigInteger[] array) {
        if (new java.math.BigInteger(String.valueOf(array.length)).compareTo(java.math.BigInteger.valueOf(1)) <= 0) {
            return ((java.math.BigInteger[])(array));
        }
        java.math.BigInteger middle_1 = new java.math.BigInteger(String.valueOf(array.length)).divide(java.math.BigInteger.valueOf(2));
        java.math.BigInteger[] left_half_1 = ((java.math.BigInteger[])(subarray(((java.math.BigInteger[])(array)), java.math.BigInteger.valueOf(0), middle_1)));
        java.math.BigInteger[] right_half_1 = ((java.math.BigInteger[])(subarray(((java.math.BigInteger[])(array)), middle_1, new java.math.BigInteger(String.valueOf(array.length)))));
        java.math.BigInteger[] sorted_left_1 = ((java.math.BigInteger[])(merge_sort(((java.math.BigInteger[])(left_half_1)))));
        java.math.BigInteger[] sorted_right_1 = ((java.math.BigInteger[])(merge_sort(((java.math.BigInteger[])(right_half_1)))));
        return ((java.math.BigInteger[])(merge(((java.math.BigInteger[])(sorted_left_1)), ((java.math.BigInteger[])(sorted_right_1)))));
    }

    static java.math.BigInteger h_index(java.math.BigInteger[] citations) {
        java.math.BigInteger idx = java.math.BigInteger.valueOf(0);
        while (idx.compareTo(new java.math.BigInteger(String.valueOf(citations.length))) < 0) {
            if (citations[_idx((citations).length, ((java.math.BigInteger)(idx)).longValue())].compareTo(java.math.BigInteger.valueOf(0)) < 0) {
                throw new RuntimeException(String.valueOf("The citations should be a list of non negative integers."));
            }
            idx = idx.add(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger[] sorted_1 = ((java.math.BigInteger[])(merge_sort(((java.math.BigInteger[])(citations)))));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(sorted_1.length));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n_1) < 0) {
            if (sorted_1[_idx((sorted_1).length, ((java.math.BigInteger)(n_1.subtract(java.math.BigInteger.valueOf(1)).subtract(i_3))).longValue())].compareTo(i_3) <= 0) {
                return i_3;
            }
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        return n_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(h_index(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5)})))));
            System.out.println(_p(h_index(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1)})))));
            System.out.println(_p(h_index(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})))));
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
