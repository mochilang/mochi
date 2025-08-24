public class Main {

    static boolean[][] make_matrix_bool(java.math.BigInteger rows, java.math.BigInteger cols, boolean init) {
        boolean[][] matrix = ((boolean[][])(new boolean[][]{}));
        for (java.math.BigInteger _v = new java.math.BigInteger(String.valueOf(0)); _v.compareTo(rows) < 0; _v = _v.add(java.math.BigInteger.ONE)) {
            boolean[] row_1 = ((boolean[])(new boolean[]{}));
            for (java.math.BigInteger _2 = new java.math.BigInteger(String.valueOf(0)); _2.compareTo(cols) < 0; _2 = _2.add(java.math.BigInteger.ONE)) {
                row_1 = ((boolean[])(appendBool(row_1, init)));
            }
            matrix = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix), java.util.stream.Stream.of(new boolean[][]{((boolean[])(row_1))})).toArray(boolean[][]::new)));
        }
        return ((boolean[][])(matrix));
    }

    static boolean match_pattern(String input_string, String pattern) {
        java.math.BigInteger len_string = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(_runeLen(input_string))).add(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger len_pattern_1 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(_runeLen(pattern))).add(java.math.BigInteger.valueOf(1))));
        boolean[][] dp_1 = ((boolean[][])(make_matrix_bool(new java.math.BigInteger(String.valueOf(len_string)), new java.math.BigInteger(String.valueOf(len_pattern_1)), false)));
        boolean[] row0_1 = ((boolean[])(dp_1[_idx((dp_1).length, 0L)]));
row0_1[(int)(0L)] = true;
dp_1[(int)(0L)] = ((boolean[])(row0_1));
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
        while (j_1.compareTo(len_pattern_1) < 0) {
            row0_1 = ((boolean[])(dp_1[_idx((dp_1).length, 0L)]));
            if ((_substr(pattern, (int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j_1)).longValue())).equals("*"))) {
row0_1[(int)(((java.math.BigInteger)(j_1)).longValue())] = row0_1[_idx((row0_1).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(2)))).longValue())];
            } else {
row0_1[(int)(((java.math.BigInteger)(j_1)).longValue())] = false;
            }
dp_1[(int)(0L)] = ((boolean[])(row0_1));
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(len_string) < 0) {
            boolean[] row_3 = ((boolean[])(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())]));
            java.math.BigInteger j2_1 = java.math.BigInteger.valueOf(1);
            while (j2_1.compareTo(len_pattern_1) < 0) {
                String s_char_1 = _substr(input_string, (int)(((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i_1)).longValue()));
                String p_char_1 = _substr(pattern, (int)(((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j2_1)).longValue()));
                if ((s_char_1.equals(p_char_1)) || (p_char_1.equals("."))) {
row_3[(int)(((java.math.BigInteger)(j2_1)).longValue())] = dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                } else                 if ((p_char_1.equals("*"))) {
                    boolean val_1 = dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())]).length, ((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(2)))).longValue())];
                    String prev_p_1 = _substr(pattern, (int)(((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(2)))).longValue()), (int)(((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()));
                    if (!val_1 && ((prev_p_1.equals(s_char_1)) || (prev_p_1.equals(".")))) {
                        val_1 = dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j2_1)).longValue())];
                    }
row_3[(int)(((java.math.BigInteger)(j2_1)).longValue())] = val_1;
                } else {
row_3[(int)(((java.math.BigInteger)(j2_1)).longValue())] = false;
                }
                j2_1 = new java.math.BigInteger(String.valueOf(j2_1.add(java.math.BigInteger.valueOf(1))));
            }
dp_1[(int)(((java.math.BigInteger)(i_1)).longValue())] = ((boolean[])(row_3));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return dp_1[_idx((dp_1).length, ((java.math.BigInteger)(len_string.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(len_string.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(len_pattern_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())];
    }

    static void main() {
        if (!(Boolean)match_pattern("aab", "c*a*b")) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (match_pattern("dabc", "*abc")) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (match_pattern("aaa", "aa")) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
        if (!(Boolean)match_pattern("aaa", "a.a")) {
            throw new RuntimeException(String.valueOf("case4 failed"));
        }
        if (match_pattern("aaab", "aa*")) {
            throw new RuntimeException(String.valueOf("case5 failed"));
        }
        if (!(Boolean)match_pattern("aaab", ".*")) {
            throw new RuntimeException(String.valueOf("case6 failed"));
        }
        if (match_pattern("a", "bbbb")) {
            throw new RuntimeException(String.valueOf("case7 failed"));
        }
        if (match_pattern("", "bbbb")) {
            throw new RuntimeException(String.valueOf("case8 failed"));
        }
        if (match_pattern("a", "")) {
            throw new RuntimeException(String.valueOf("case9 failed"));
        }
        if (!(Boolean)match_pattern("", "")) {
            throw new RuntimeException(String.valueOf("case10 failed"));
        }
        System.out.println(_p(match_pattern("aab", "c*a*b")));
        System.out.println(_p(match_pattern("dabc", "*abc")));
        System.out.println(_p(match_pattern("aaa", "aa")));
        System.out.println(_p(match_pattern("aaa", "a.a")));
        System.out.println(_p(match_pattern("aaab", "aa*")));
        System.out.println(_p(match_pattern("aaab", ".*")));
        System.out.println(_p(match_pattern("a", "bbbb")));
        System.out.println(_p(match_pattern("", "bbbb")));
        System.out.println(_p(match_pattern("a", "")));
        System.out.println(_p(match_pattern("", "")));
    }
    public static void main(String[] args) {
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
