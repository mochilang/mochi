public class Main {

    static java.math.BigInteger dp_count(java.math.BigInteger[] s, java.math.BigInteger n) {
        if (n.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            return java.math.BigInteger.valueOf(0);
        }
        java.math.BigInteger[] table_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) <= 0) {
            table_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(table_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
table_1[(int)(0L)] = java.math.BigInteger.valueOf(1);
        java.math.BigInteger idx_1 = java.math.BigInteger.valueOf(0);
        while (idx_1.compareTo(new java.math.BigInteger(String.valueOf(s.length))) < 0) {
            java.math.BigInteger coin_val_1 = new java.math.BigInteger(String.valueOf(s[_idx((s).length, ((java.math.BigInteger)(idx_1)).longValue())]));
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(coin_val_1));
            while (j_1.compareTo(n) <= 0) {
table_1[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(j_1)).longValue())].add(table_1[_idx((table_1).length, ((java.math.BigInteger)(j_1.subtract(coin_val_1))).longValue())])));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            idx_1 = new java.math.BigInteger(String.valueOf(idx_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(table_1[_idx((table_1).length, ((java.math.BigInteger)(n)).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(dp_count(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})), java.math.BigInteger.valueOf(4)));
        System.out.println(dp_count(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})), java.math.BigInteger.valueOf(7)));
        System.out.println(dp_count(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(6)})), java.math.BigInteger.valueOf(10)));
        System.out.println(dp_count(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(10)})), java.math.BigInteger.valueOf(99)));
        System.out.println(dp_count(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6)})), java.math.BigInteger.valueOf(0)));
        System.out.println(dp_count(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(5)).negate()))));
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
