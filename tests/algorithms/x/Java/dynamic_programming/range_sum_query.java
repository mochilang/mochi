public class Main {
    static class Query {
        java.math.BigInteger left;
        java.math.BigInteger right;
        Query(java.math.BigInteger left, java.math.BigInteger right) {
            this.left = left;
            this.right = right;
        }
        Query() {}
        @Override public String toString() {
            return String.format("{'left': %s, 'right': %s}", String.valueOf(left), String.valueOf(right));
        }
    }

    static java.math.BigInteger[] arr1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(61), java.math.BigInteger.valueOf(12)}));
    static Query[] queries1;
    static java.math.BigInteger[] arr2 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(3)}));
    static Query[] queries2;

    static java.math.BigInteger[] prefix_sum(java.math.BigInteger[] arr, Query[] queries) {
        java.math.BigInteger[] dp = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            if (i_1.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                dp = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(arr[_idx((arr).length, 0L)])))).toArray(java.math.BigInteger[]::new)));
            } else {
                dp = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(dp[_idx((dp).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(arr[_idx((arr).length, ((java.math.BigInteger)(i_1)).longValue())]))))).toArray(java.math.BigInteger[]::new)));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
        while (j_1.compareTo(new java.math.BigInteger(String.valueOf(queries.length))) < 0) {
            Query q_1 = queries[_idx((queries).length, ((java.math.BigInteger)(j_1)).longValue())];
            java.math.BigInteger sum_1 = new java.math.BigInteger(String.valueOf(dp[_idx((dp).length, ((java.math.BigInteger)(q_1.right)).longValue())]));
            if (q_1.left.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                sum_1 = new java.math.BigInteger(String.valueOf(sum_1.subtract(dp[_idx((dp).length, ((java.math.BigInteger)(q_1.left.subtract(java.math.BigInteger.valueOf(1)))).longValue())])));
            }
            result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(sum_1)))).toArray(java.math.BigInteger[]::new)));
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(result_1));
    }
    public static void main(String[] args) {
        queries1 = ((Query[])(new Query[]{new Query(java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5)), new Query(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5)), new Query(java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4))}));
        System.out.println(_p(prefix_sum(((java.math.BigInteger[])(arr1)), ((Query[])(queries1)))));
        queries2 = ((Query[])(new Query[]{new Query(java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)), new Query(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3)), new Query(java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2))}));
        System.out.println(_p(prefix_sum(((java.math.BigInteger[])(arr2)), ((Query[])(queries2)))));
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
