public class Main {

    static boolean[] make_bool_list(java.math.BigInteger n) {
        boolean[] row = ((boolean[])(new boolean[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            row = ((boolean[])(appendBool(row, false)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((boolean[])(row));
    }

    static boolean[][] make_bool_matrix(java.math.BigInteger rows, java.math.BigInteger cols) {
        boolean[][] matrix = ((boolean[][])(new boolean[][]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(rows) < 0) {
            matrix = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix), java.util.stream.Stream.of(new boolean[][]{((boolean[])(make_bool_list(new java.math.BigInteger(String.valueOf(cols)))))})).toArray(boolean[][]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((boolean[][])(matrix));
    }

    static boolean is_match(String s, String p) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(_runeLen(s)));
        java.math.BigInteger m_1 = new java.math.BigInteger(String.valueOf(_runeLen(p)));
        boolean[][] dp_1 = ((boolean[][])(make_bool_matrix(new java.math.BigInteger(String.valueOf(n.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(m_1.add(java.math.BigInteger.valueOf(1)))))));
dp_1[_idx((dp_1).length, 0L)][(int)(0L)] = true;
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
        while (j_1.compareTo(m_1) <= 0) {
            if ((_substr(p, (int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j_1)).longValue())).equals("*"))) {
dp_1[_idx((dp_1).length, 0L)][(int)(((java.math.BigInteger)(j_1)).longValue())] = dp_1[_idx((dp_1).length, 0L)][_idx((dp_1[_idx((dp_1).length, 0L)]).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())];
            }
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(1);
        while (i_5.compareTo(n) <= 0) {
            java.math.BigInteger j2_1 = java.math.BigInteger.valueOf(1);
            while (j2_1.compareTo(m_1) <= 0) {
                String pc_1 = _substr(p, (int)(((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j2_1)).longValue()));
                String sc_1 = _substr(s, (int)(((java.math.BigInteger)(i_5.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i_5)).longValue()));
                if ((pc_1.equals(sc_1)) || (pc_1.equals("?"))) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_5)).longValue())][(int)(((java.math.BigInteger)(j2_1)).longValue())] = dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_5.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_5.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())];
                } else                 if ((pc_1.equals("*"))) {
                    if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_5.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_5.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j2_1)).longValue())] || dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_5)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_5)).longValue())]).length, ((java.math.BigInteger)(j2_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_5)).longValue())][(int)(((java.math.BigInteger)(j2_1)).longValue())] = true;
                    }
                }
                j2_1 = new java.math.BigInteger(String.valueOf(j2_1.add(java.math.BigInteger.valueOf(1))));
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return dp_1[_idx((dp_1).length, ((java.math.BigInteger)(n)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(n)).longValue())]).length, ((java.math.BigInteger)(m_1)).longValue())];
    }

    static void print_bool(boolean b) {
        if (b) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        print_bool(is_match("abc", "a*c"));
        print_bool(is_match("abc", "a*d"));
        print_bool(is_match("baaabab", "*****ba*****ab"));
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
