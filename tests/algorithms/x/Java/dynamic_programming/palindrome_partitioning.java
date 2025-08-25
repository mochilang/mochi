public class Main {

    static java.math.BigInteger min_partitions(String s) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(_runeLen(s)));
        java.math.BigInteger[] cut_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            cut_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cut_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        boolean[][] pal_1 = ((boolean[][])(new boolean[][]{}));
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            boolean[] row_1 = ((boolean[])(new boolean[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(n) < 0) {
                row_1 = ((boolean[])(appendBool(row_1, false)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            pal_1 = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(pal_1), java.util.stream.Stream.of(new boolean[][]{((boolean[])(row_1))})).toArray(boolean[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            java.math.BigInteger mincut_1 = new java.math.BigInteger(String.valueOf(i_1));
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
            while (j_3.compareTo(i_1) <= 0) {
                if ((s.substring((int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1)).longValue())+1).equals(s.substring((int)(((java.math.BigInteger)(j_3)).longValue()), (int)(((java.math.BigInteger)(j_3)).longValue())+1))) && (i_1.subtract(j_3).compareTo(java.math.BigInteger.valueOf(2)) < 0 || pal_1[_idx((pal_1).length, ((java.math.BigInteger)(j_3.add(java.math.BigInteger.valueOf(1)))).longValue())][_idx((pal_1[_idx((pal_1).length, ((java.math.BigInteger)(j_3.add(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())])) {
pal_1[_idx((pal_1).length, ((java.math.BigInteger)(j_3)).longValue())][(int)(((java.math.BigInteger)(i_1)).longValue())] = true;
                    if (j_3.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                        mincut_1 = java.math.BigInteger.valueOf(0);
                    } else {
                        java.math.BigInteger candidate_1 = new java.math.BigInteger(String.valueOf(cut_1[_idx((cut_1).length, ((java.math.BigInteger)(j_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(java.math.BigInteger.valueOf(1))));
                        if (candidate_1.compareTo(mincut_1) < 0) {
                            mincut_1 = new java.math.BigInteger(String.valueOf(candidate_1));
                        }
                    }
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
cut_1[(int)(((java.math.BigInteger)(i_1)).longValue())] = new java.math.BigInteger(String.valueOf(mincut_1));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(cut_1[_idx((cut_1).length, ((java.math.BigInteger)(n.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(min_partitions("aab"));
        System.out.println(min_partitions("aaa"));
        System.out.println(min_partitions("ababbbabbababa"));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
