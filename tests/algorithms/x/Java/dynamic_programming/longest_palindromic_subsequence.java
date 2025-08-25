public class Main {

    static String reverse(String s) {
        String result = "";
        java.math.BigInteger i_1 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(_runeLen(s))).subtract(java.math.BigInteger.valueOf(1))));
        while (i_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            result = result + _substr(s, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue()));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
        }
        return result;
    }

    static java.math.BigInteger max_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) > 0) {
            return new java.math.BigInteger(String.valueOf(a));
        }
        return new java.math.BigInteger(String.valueOf(b));
    }

    static java.math.BigInteger longest_palindromic_subsequence(String s) {
        String rev = String.valueOf(reverse(s));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(_runeLen(s)));
        java.math.BigInteger m_1 = new java.math.BigInteger(String.valueOf(_runeLen(rev)));
        java.math.BigInteger[][] dp_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n_1) <= 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(m_1) <= 0) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            dp_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(n_1) <= 0) {
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(1);
            while (j_3.compareTo(m_1) <= 0) {
                String a_char_1 = _substr(s, (int)(((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i_3)).longValue()));
                String b_char_1 = _substr(rev, (int)(((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j_3)).longValue()));
                if ((a_char_1.equals(b_char_1))) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(1).add(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())])));
                } else {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = new java.math.BigInteger(String.valueOf(max_int(new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())])), new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())])))));
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(n_1)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(n_1)).longValue())]).length, ((java.math.BigInteger)(m_1)).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(_p(longest_palindromic_subsequence("bbbab")));
        System.out.println(_p(longest_palindromic_subsequence("bbabcbcab")));
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
