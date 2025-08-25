public class Main {

    static boolean recursive_match(String text, String pattern) {
        if (new java.math.BigInteger(String.valueOf(_runeLen(pattern))).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return new java.math.BigInteger(String.valueOf(_runeLen(text))).compareTo(java.math.BigInteger.valueOf(0)) == 0;
        }
        if (new java.math.BigInteger(String.valueOf(_runeLen(text))).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            if (new java.math.BigInteger(String.valueOf(_runeLen(pattern))).compareTo(java.math.BigInteger.valueOf(2)) >= 0 && (_substr(pattern, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(pattern))).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(_runeLen(pattern)))).equals("*"))) {
                return recursive_match(text, _substr(pattern, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(pattern))).subtract(java.math.BigInteger.valueOf(2)))).longValue())));
            }
            return false;
        }
        String last_text_1 = _substr(text, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(text))).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(_runeLen(text))));
        String last_pattern_1 = _substr(pattern, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(pattern))).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(_runeLen(pattern))));
        if ((last_text_1.equals(last_pattern_1)) || (last_pattern_1.equals("."))) {
            return recursive_match(_substr(text, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(text))).subtract(java.math.BigInteger.valueOf(1)))).longValue())), _substr(pattern, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(pattern))).subtract(java.math.BigInteger.valueOf(1)))).longValue())));
        }
        if ((last_pattern_1.equals("*"))) {
            if (recursive_match(_substr(text, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(text))).subtract(java.math.BigInteger.valueOf(1)))).longValue())), pattern)) {
                return true;
            }
            return recursive_match(text, _substr(pattern, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(pattern))).subtract(java.math.BigInteger.valueOf(2)))).longValue())));
        }
        return false;
    }

    static boolean dp_match(String text, String pattern) {
        java.math.BigInteger m = new java.math.BigInteger(String.valueOf(_runeLen(text)));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(_runeLen(pattern)));
        boolean[][] dp_1 = ((boolean[][])(new boolean[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(m) <= 0) {
            boolean[] row_1 = ((boolean[])(new boolean[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(n_1) <= 0) {
                row_1 = ((boolean[])(appendBool(row_1, false)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            dp_1 = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1), java.util.stream.Stream.of(new boolean[][]{((boolean[])(row_1))})).toArray(boolean[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
dp_1[_idx((dp_1).length, 0L)][(int)(0L)] = true;
        java.math.BigInteger j_3 = java.math.BigInteger.valueOf(1);
        while (j_3.compareTo(n_1) <= 0) {
            if ((_substr(pattern, (int)(((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j_3)).longValue())).equals("*")) && j_3.compareTo(java.math.BigInteger.valueOf(2)) >= 0) {
                if (dp_1[_idx((dp_1).length, 0L)][_idx((dp_1[_idx((dp_1).length, 0L)]).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(2)))).longValue())]) {
dp_1[_idx((dp_1).length, 0L)][(int)(((java.math.BigInteger)(j_3)).longValue())] = true;
                }
            }
            j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(m) <= 0) {
            j_3 = java.math.BigInteger.valueOf(1);
            while (j_3.compareTo(n_1) <= 0) {
                String p_char_1 = _substr(pattern, (int)(((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j_3)).longValue()));
                String t_char_1 = _substr(text, (int)(((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i_1)).longValue()));
                if ((p_char_1.equals(".")) || (p_char_1.equals(t_char_1))) {
                    if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = true;
                    }
                } else                 if ((p_char_1.equals("*"))) {
                    if (j_3.compareTo(java.math.BigInteger.valueOf(2)) >= 0) {
                        if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(2)))).longValue())]) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = true;
                        }
                        String prev_p_1 = _substr(pattern, (int)(((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(2)))).longValue()), (int)(((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue()));
                        if ((prev_p_1.equals(".")) || (prev_p_1.equals(t_char_1))) {
                            if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())]) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = true;
                            }
                        }
                    }
                } else {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = false;
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return dp_1[_idx((dp_1).length, ((java.math.BigInteger)(m)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(m)).longValue())]).length, ((java.math.BigInteger)(n_1)).longValue())];
    }

    static void print_bool(boolean b) {
        if (b) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        print_bool(recursive_match("abc", "a.c"));
        print_bool(recursive_match("abc", "af*.c"));
        print_bool(recursive_match("abc", "a.c*"));
        print_bool(recursive_match("abc", "a.c*d"));
        print_bool(recursive_match("aa", ".*"));
        print_bool(dp_match("abc", "a.c"));
        print_bool(dp_match("abc", "af*.c"));
        print_bool(dp_match("abc", "a.c*"));
        print_bool(dp_match("abc", "a.c*d"));
        print_bool(dp_match("aa", ".*"));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
