public class Main {

    static java.math.BigInteger[] sort_list(java.math.BigInteger[] nums) {
        java.math.BigInteger[] arr = ((java.math.BigInteger[])(nums));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            java.math.BigInteger key_1 = new java.math.BigInteger(String.valueOf(arr[_idx((arr).length, ((java.math.BigInteger)(i_1)).longValue())]));
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
            while (j_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0 && arr[_idx((arr).length, ((java.math.BigInteger)(j_1)).longValue())].compareTo(key_1) > 0) {
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = new java.math.BigInteger(String.valueOf(arr[_idx((arr).length, ((java.math.BigInteger)(j_1)).longValue())]));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
            }
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = new java.math.BigInteger(String.valueOf(key_1));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(arr));
    }

    static java.math.BigInteger[] largest_divisible_subset(java.math.BigInteger[] items) {
        if (new java.math.BigInteger(String.valueOf(items.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        }
        java.math.BigInteger[] nums_1 = ((java.math.BigInteger[])(sort_list(((java.math.BigInteger[])(items)))));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(nums_1.length));
        java.math.BigInteger[] memo_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] prev_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n_1) < 0) {
            memo_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(memo_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1))).toArray(java.math.BigInteger[]::new)));
            prev_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(prev_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(i_3)))).toArray(java.math.BigInteger[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n_1) < 0) {
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
            while (j_3.compareTo(i_3) < 0) {
                if ((nums_1[_idx((nums_1).length, ((java.math.BigInteger)(j_3)).longValue())].compareTo(java.math.BigInteger.valueOf(0)) == 0 || nums_1[_idx((nums_1).length, ((java.math.BigInteger)(i_3)).longValue())].remainder(nums_1[_idx((nums_1).length, ((java.math.BigInteger)(j_3)).longValue())]).compareTo(java.math.BigInteger.valueOf(0)) == 0) && memo_1[_idx((memo_1).length, ((java.math.BigInteger)(j_3)).longValue())].add(java.math.BigInteger.valueOf(1)).compareTo(memo_1[_idx((memo_1).length, ((java.math.BigInteger)(i_3)).longValue())]) > 0) {
memo_1[(int)(((java.math.BigInteger)(i_3)).longValue())] = new java.math.BigInteger(String.valueOf(memo_1[_idx((memo_1).length, ((java.math.BigInteger)(j_3)).longValue())].add(java.math.BigInteger.valueOf(1))));
prev_1[(int)(((java.math.BigInteger)(i_3)).longValue())] = new java.math.BigInteger(String.valueOf(j_3));
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger ans_1 = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
        java.math.BigInteger last_index_1 = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
        i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n_1) < 0) {
            if (memo_1[_idx((memo_1).length, ((java.math.BigInteger)(i_3)).longValue())].compareTo(ans_1) > 0) {
                ans_1 = new java.math.BigInteger(String.valueOf(memo_1[_idx((memo_1).length, ((java.math.BigInteger)(i_3)).longValue())]));
                last_index_1 = new java.math.BigInteger(String.valueOf(i_3));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        if (last_index_1.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            return ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        }
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(nums_1[_idx((nums_1).length, ((java.math.BigInteger)(last_index_1)).longValue())]))}));
        while (prev_1[_idx((prev_1).length, ((java.math.BigInteger)(last_index_1)).longValue())].compareTo(last_index_1) != 0) {
            last_index_1 = new java.math.BigInteger(String.valueOf(prev_1[_idx((prev_1).length, ((java.math.BigInteger)(last_index_1)).longValue())]));
            result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(nums_1[_idx((nums_1).length, ((java.math.BigInteger)(last_index_1)).longValue())])))).toArray(java.math.BigInteger[]::new)));
        }
        return ((java.math.BigInteger[])(result_1));
    }

    static void main() {
        java.math.BigInteger[] items = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(16), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(4)}));
        java.math.BigInteger[] subset_1 = ((java.math.BigInteger[])(largest_divisible_subset(((java.math.BigInteger[])(items)))));
        System.out.println("The longest divisible subset of " + _p(items) + " is " + _p(subset_1) + ".");
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
