public class Main {

    static java.math.BigInteger[] copy_list(java.math.BigInteger[] src) {
        java.math.BigInteger[] result = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(src.length))) < 0) {
            result = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(src[_idx((src).length, ((java.math.BigInteger)(i_1)).longValue())])))).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(result));
    }

    static java.math.BigInteger[][] subset_combinations(java.math.BigInteger[] elements, java.math.BigInteger n) {
        java.math.BigInteger r = new java.math.BigInteger(String.valueOf(elements.length));
        if (n.compareTo(r) > 0) {
            return ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        }
        java.math.BigInteger[][][] dp_1 = ((java.math.BigInteger[][][])(new java.math.BigInteger[][][]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(r) <= 0) {
            dp_1 = ((java.math.BigInteger[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1), java.util.stream.Stream.of(new java.math.BigInteger[][][]{((java.math.BigInteger[][])(new java.math.BigInteger[][]{}))})).toArray(java.math.BigInteger[][][]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
dp_1[(int)(0L)] = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1[_idx((dp_1).length, 0L)]), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{}))})).toArray(java.math.BigInteger[][]::new)));
        i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(r) <= 0) {
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(i_3));
            while (j_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                java.math.BigInteger[][] prevs_1 = ((java.math.BigInteger[][])(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
                java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
                while (k_1.compareTo(new java.math.BigInteger(String.valueOf(prevs_1.length))) < 0) {
                    java.math.BigInteger[] prev_1 = ((java.math.BigInteger[])(prevs_1[_idx((prevs_1).length, ((java.math.BigInteger)(k_1)).longValue())]));
                    java.math.BigInteger[] comb_1 = ((java.math.BigInteger[])(copy_list(((java.math.BigInteger[])(prev_1)))));
                    comb_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(comb_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(elements[_idx((elements).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())])))).toArray(java.math.BigInteger[]::new)));
dp_1[(int)(((java.math.BigInteger)(j_1)).longValue())] = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(j_1)).longValue())]), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(comb_1))})).toArray(java.math.BigInteger[][]::new)));
                    k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
                }
                j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[][])(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(n)).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(_p(subset_combinations(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(20), java.math.BigInteger.valueOf(30), java.math.BigInteger.valueOf(40)})), java.math.BigInteger.valueOf(2))));
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
