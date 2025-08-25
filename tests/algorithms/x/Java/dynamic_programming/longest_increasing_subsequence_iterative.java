public class Main {

    static java.math.BigInteger[] copy_list(java.math.BigInteger[] xs) {
        java.math.BigInteger[] res = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            res = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(xs[_idx((xs).length, ((java.math.BigInteger)(i_1)).longValue())])))).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(res));
    }

    static java.math.BigInteger[] longest_subsequence(java.math.BigInteger[] arr) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(arr.length));
        java.math.BigInteger[][] lis_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n) < 0) {
            java.math.BigInteger[] single_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            single_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(single_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(arr[_idx((arr).length, ((java.math.BigInteger)(i_3)).longValue())])))).toArray(java.math.BigInteger[]::new)));
            lis_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(lis_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(single_1))})).toArray(java.math.BigInteger[][]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(n) < 0) {
            java.math.BigInteger prev_1 = java.math.BigInteger.valueOf(0);
            while (prev_1.compareTo(i_3) < 0) {
                if (arr[_idx((arr).length, ((java.math.BigInteger)(prev_1)).longValue())].compareTo(arr[_idx((arr).length, ((java.math.BigInteger)(i_3)).longValue())]) <= 0 && new java.math.BigInteger(String.valueOf(lis_1[_idx((lis_1).length, ((java.math.BigInteger)(prev_1)).longValue())].length)).add(java.math.BigInteger.valueOf(1)).compareTo(new java.math.BigInteger(String.valueOf(lis_1[_idx((lis_1).length, ((java.math.BigInteger)(i_3)).longValue())].length))) > 0) {
                    java.math.BigInteger[] temp_1 = ((java.math.BigInteger[])(copy_list(((java.math.BigInteger[])(lis_1[_idx((lis_1).length, ((java.math.BigInteger)(prev_1)).longValue())])))));
                    java.math.BigInteger[] temp2_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(arr[_idx((arr).length, ((java.math.BigInteger)(i_3)).longValue())])))).toArray(java.math.BigInteger[]::new)));
lis_1[(int)(((java.math.BigInteger)(i_3)).longValue())] = ((java.math.BigInteger[])(temp2_1));
                }
                prev_1 = new java.math.BigInteger(String.valueOf(prev_1.add(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n) < 0) {
            if (new java.math.BigInteger(String.valueOf(lis_1[_idx((lis_1).length, ((java.math.BigInteger)(i_3)).longValue())].length)).compareTo(new java.math.BigInteger(String.valueOf(result_1.length))) > 0) {
                result_1 = ((java.math.BigInteger[])(lis_1[_idx((lis_1).length, ((java.math.BigInteger)(i_3)).longValue())]));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(result_1));
    }

    static void main() {
        System.out.println(_p(longest_subsequence(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(22), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(33), java.math.BigInteger.valueOf(21), java.math.BigInteger.valueOf(50), java.math.BigInteger.valueOf(41), java.math.BigInteger.valueOf(60), java.math.BigInteger.valueOf(80)})))));
        System.out.println(_p(longest_subsequence(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(12), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(9)})))));
        System.out.println(_p(longest_subsequence(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(7)})))));
        System.out.println(_p(longest_subsequence(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(28), java.math.BigInteger.valueOf(26), java.math.BigInteger.valueOf(12), java.math.BigInteger.valueOf(23), java.math.BigInteger.valueOf(35), java.math.BigInteger.valueOf(39)})))));
        System.out.println(_p(longest_subsequence(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1)})))));
        System.out.println(_p(longest_subsequence(((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
    }
    public static void main(String[] args) {
        main();
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
