public class Main {

    static java.math.BigInteger find_min(java.math.BigInteger[] numbers) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(numbers.length));
        java.math.BigInteger s_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger idx_1 = java.math.BigInteger.valueOf(0);
        while (idx_1.compareTo(n) < 0) {
            s_1 = new java.math.BigInteger(String.valueOf(s_1.add(numbers[_idx((numbers).length, ((java.math.BigInteger)(idx_1)).longValue())])));
            idx_1 = new java.math.BigInteger(String.valueOf(idx_1.add(java.math.BigInteger.valueOf(1))));
        }
        boolean[][] dp_1 = ((boolean[][])(new boolean[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) <= 0) {
            boolean[] row_1 = ((boolean[])(new boolean[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(s_1) <= 0) {
                row_1 = ((boolean[])(appendBool(row_1, false)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            dp_1 = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1), java.util.stream.Stream.of(new boolean[][]{((boolean[])(row_1))})).toArray(boolean[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) <= 0) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(0L)] = true;
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger j_3 = java.math.BigInteger.valueOf(1);
        while (j_3.compareTo(s_1) <= 0) {
dp_1[_idx((dp_1).length, 0L)][(int)(((java.math.BigInteger)(j_3)).longValue())] = false;
            j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(n) <= 0) {
            j_3 = java.math.BigInteger.valueOf(1);
            while (j_3.compareTo(s_1) <= 0) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())];
                if (numbers[_idx((numbers).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].compareTo(j_3) <= 0) {
                    if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(numbers[_idx((numbers).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]))).longValue())]) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = true;
                    }
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger diff_1 = java.math.BigInteger.valueOf(0);
        j_3 = new java.math.BigInteger(String.valueOf(s_1.divide(java.math.BigInteger.valueOf(2))));
        while (j_3.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(n)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(n)).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())]) {
                diff_1 = new java.math.BigInteger(String.valueOf(s_1.subtract(java.math.BigInteger.valueOf(2).multiply(j_3))));
                break;
            }
            j_3 = new java.math.BigInteger(String.valueOf(j_3.subtract(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(diff_1));
    }
    public static void main(String[] args) {
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(5)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(5)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(5)).negate())), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(1)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(9)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(3)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1)})))));
        System.out.println(_p(find_min(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)})))));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
