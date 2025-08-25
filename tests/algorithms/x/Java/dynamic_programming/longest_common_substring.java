public class Main {

    static String longest_common_substring(String text1, String text2) {
        if (new java.math.BigInteger(String.valueOf(_runeLen(text1))).compareTo(java.math.BigInteger.valueOf(0)) == 0 || new java.math.BigInteger(String.valueOf(_runeLen(text2))).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return "";
        }
        java.math.BigInteger m_1 = new java.math.BigInteger(String.valueOf(_runeLen(text1)));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(_runeLen(text2)));
        java.math.BigInteger[][] dp_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(m_1.add(java.math.BigInteger.valueOf(1))) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(n_1.add(java.math.BigInteger.valueOf(1))) < 0) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            dp_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger end_pos_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger max_len_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger ii_1 = java.math.BigInteger.valueOf(1);
        while (ii_1.compareTo(m_1) <= 0) {
            java.math.BigInteger jj_1 = java.math.BigInteger.valueOf(1);
            while (jj_1.compareTo(n_1) <= 0) {
                if ((_substr(text1, (int)(((java.math.BigInteger)(ii_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(ii_1)).longValue())).equals(_substr(text2, (int)(((java.math.BigInteger)(jj_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(jj_1)).longValue()))))) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(ii_1)).longValue())][(int)(((java.math.BigInteger)(jj_1)).longValue())] = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(1).add(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(ii_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(ii_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(jj_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())])));
                    if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(ii_1)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(ii_1)).longValue())]).length, ((java.math.BigInteger)(jj_1)).longValue())].compareTo(max_len_1) > 0) {
                        max_len_1 = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(ii_1)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(ii_1)).longValue())]).length, ((java.math.BigInteger)(jj_1)).longValue())]));
                        end_pos_1 = new java.math.BigInteger(String.valueOf(ii_1));
                    }
                }
                jj_1 = new java.math.BigInteger(String.valueOf(jj_1.add(java.math.BigInteger.valueOf(1))));
            }
            ii_1 = new java.math.BigInteger(String.valueOf(ii_1.add(java.math.BigInteger.valueOf(1))));
        }
        return _substr(text1, (int)(((java.math.BigInteger)(end_pos_1.subtract(max_len_1))).longValue()), (int)(((java.math.BigInteger)(end_pos_1)).longValue()));
    }
    public static void main(String[] args) {
        System.out.println(longest_common_substring("abcdef", "xabded"));
        System.out.println("\n");
        System.out.println(longest_common_substring("zxabcdezy", "yzabcdezx"));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
