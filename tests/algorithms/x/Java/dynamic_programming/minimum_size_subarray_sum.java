public class Main {

    static java.math.BigInteger minimum_subarray_sum(java.math.BigInteger target, java.math.BigInteger[] numbers) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(numbers.length));
        if (n.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return java.math.BigInteger.valueOf(0);
        }
        if (target.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
            while (i_1.compareTo(n) < 0) {
                if (numbers[_idx((numbers).length, ((java.math.BigInteger)(i_1)).longValue())].compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                    return java.math.BigInteger.valueOf(0);
                }
                i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
            }
        }
        java.math.BigInteger left_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger right_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger curr_sum_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger min_len_1 = new java.math.BigInteger(String.valueOf(n.add(java.math.BigInteger.valueOf(1))));
        while (right_1.compareTo(n) < 0) {
            curr_sum_1 = new java.math.BigInteger(String.valueOf(curr_sum_1.add(numbers[_idx((numbers).length, ((java.math.BigInteger)(right_1)).longValue())])));
            while (curr_sum_1.compareTo(target) >= 0 && left_1.compareTo(right_1) <= 0) {
                java.math.BigInteger current_len_1 = new java.math.BigInteger(String.valueOf(right_1.subtract(left_1).add(java.math.BigInteger.valueOf(1))));
                if (current_len_1.compareTo(min_len_1) < 0) {
                    min_len_1 = new java.math.BigInteger(String.valueOf(current_len_1));
                }
                curr_sum_1 = new java.math.BigInteger(String.valueOf(curr_sum_1.subtract(numbers[_idx((numbers).length, ((java.math.BigInteger)(left_1)).longValue())])));
                left_1 = new java.math.BigInteger(String.valueOf(left_1.add(java.math.BigInteger.valueOf(1))));
            }
            right_1 = new java.math.BigInteger(String.valueOf(right_1.add(java.math.BigInteger.valueOf(1))));
        }
        if (min_len_1.compareTo(n.add(java.math.BigInteger.valueOf(1))) == 0) {
            return java.math.BigInteger.valueOf(0);
        }
        return new java.math.BigInteger(String.valueOf(min_len_1));
    }
    public static void main(String[] args) {
        System.out.println(_p(minimum_subarray_sum(java.math.BigInteger.valueOf(7), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(3)})))));
        System.out.println(_p(minimum_subarray_sum(java.math.BigInteger.valueOf(7), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(3)).negate()))})))));
        System.out.println(_p(minimum_subarray_sum(java.math.BigInteger.valueOf(11), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1)})))));
        System.out.println(_p(minimum_subarray_sum(java.math.BigInteger.valueOf(0), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})))));
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
