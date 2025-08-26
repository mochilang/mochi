public class Main {
    static String[] sample_input = ((String[])(new String[]{"1010110010 10110", "1110111011 10011"}));

    static java.math.BigInteger is_substring(String a, String b) {
        java.math.BigInteger la = new java.math.BigInteger(String.valueOf(_runeLen(a)));
        java.math.BigInteger lb_1 = new java.math.BigInteger(String.valueOf(_runeLen(b)));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.add(lb_1).compareTo(la) <= 0) {
            if ((_substr(a, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(lb_1))).longValue())).equals(b))) {
                return java.math.BigInteger.valueOf(1);
            }
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return java.math.BigInteger.valueOf(0);
    }

    static java.math.BigInteger[] solve(String[] lines) {
        java.math.BigInteger[] res = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        for (String line : lines) {
            String[] parts_1 = ((String[])(new String[]{}));
            String cur_1 = "";
            java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
            while (i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(line)))) < 0) {
                String ch_1 = _substr(line, (int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue()));
                if ((ch_1.equals(" "))) {
                    parts_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts_1), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
                    cur_1 = "";
                } else {
                    cur_1 = cur_1 + ch_1;
                }
                i_3 = i_3.add(java.math.BigInteger.valueOf(1));
            }
            parts_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts_1), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
            String a_1 = parts_1[_idx((parts_1).length, 0L)];
            String b_1 = parts_1[_idx((parts_1).length, 1L)];
            res = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(is_substring(a_1, b_1))).toArray(java.math.BigInteger[]::new)));
        }
        return ((java.math.BigInteger[])(res));
    }
    public static void main(String[] args) {
        for (java.math.BigInteger r : solve(((String[])(sample_input)))) {
            System.out.println(r);
        }
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
