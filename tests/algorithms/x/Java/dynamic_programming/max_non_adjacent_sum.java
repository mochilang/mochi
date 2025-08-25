public class Main {

    static java.math.BigInteger maximum_non_adjacent_sum(java.math.BigInteger[] nums) {
        if (new java.math.BigInteger(String.valueOf(nums.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return java.math.BigInteger.valueOf(0);
        }
        java.math.BigInteger max_including_1 = new java.math.BigInteger(String.valueOf(nums[_idx((nums).length, 0L)]));
        java.math.BigInteger max_excluding_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(nums.length))) < 0) {
            java.math.BigInteger num_1 = new java.math.BigInteger(String.valueOf(nums[_idx((nums).length, ((java.math.BigInteger)(i_1)).longValue())]));
            java.math.BigInteger new_including_1 = new java.math.BigInteger(String.valueOf(max_excluding_1.add(num_1)));
            java.math.BigInteger new_excluding_1 = new java.math.BigInteger(String.valueOf(max_including_1.compareTo(max_excluding_1) > 0 ? max_including_1 : max_excluding_1));
            max_including_1 = new java.math.BigInteger(String.valueOf(new_including_1));
            max_excluding_1 = new java.math.BigInteger(String.valueOf(new_excluding_1));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        if (max_including_1.compareTo(max_excluding_1) > 0) {
            return new java.math.BigInteger(String.valueOf(max_including_1));
        }
        return new java.math.BigInteger(String.valueOf(max_excluding_1));
    }
    public static void main(String[] args) {
        System.out.println(_p(maximum_non_adjacent_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})))));
        System.out.println(_p(maximum_non_adjacent_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(6)})))));
        System.out.println(_p(maximum_non_adjacent_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(5)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(3)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(7)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(6)).negate()))})))));
        System.out.println(_p(maximum_non_adjacent_sum(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(499), java.math.BigInteger.valueOf(500), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(3)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(7)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(6)).negate()))})))));
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
