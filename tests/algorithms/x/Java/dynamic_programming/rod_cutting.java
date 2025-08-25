public class Main {
    static java.math.BigInteger[] prices = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(17), java.math.BigInteger.valueOf(17), java.math.BigInteger.valueOf(20), java.math.BigInteger.valueOf(24), java.math.BigInteger.valueOf(30)}));

    static void enforce_args(java.math.BigInteger n, java.math.BigInteger[] prices) {
        if (n.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            throw new RuntimeException(String.valueOf("n must be non-negative"));
        }
        if (n.compareTo(new java.math.BigInteger(String.valueOf(prices.length))) > 0) {
            throw new RuntimeException(String.valueOf("price list is shorter than n"));
        }
    }

    static java.math.BigInteger bottom_up_cut_rod(java.math.BigInteger n, java.math.BigInteger[] prices) {
        enforce_args(new java.math.BigInteger(String.valueOf(n)), ((java.math.BigInteger[])(prices)));
        java.math.BigInteger[] max_rev_1 = new java.math.BigInteger[0];
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) <= 0) {
            if (i_1.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                max_rev_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(max_rev_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            } else {
                max_rev_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(max_rev_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2147483648L)).negate())))).toArray(java.math.BigInteger[]::new)));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger length_1 = java.math.BigInteger.valueOf(1);
        while (length_1.compareTo(n) <= 0) {
            java.math.BigInteger best_1 = new java.math.BigInteger(String.valueOf(max_rev_1[_idx((max_rev_1).length, ((java.math.BigInteger)(length_1)).longValue())]));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
            while (j_1.compareTo(length_1) <= 0) {
                java.math.BigInteger candidate_1 = new java.math.BigInteger(String.valueOf(prices[_idx((prices).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(max_rev_1[_idx((max_rev_1).length, ((java.math.BigInteger)(length_1.subtract(j_1))).longValue())])));
                if (candidate_1.compareTo(best_1) > 0) {
                    best_1 = new java.math.BigInteger(String.valueOf(candidate_1));
                }
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
max_rev_1[(int)(((java.math.BigInteger)(length_1)).longValue())] = new java.math.BigInteger(String.valueOf(best_1));
            length_1 = new java.math.BigInteger(String.valueOf(length_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(max_rev_1[_idx((max_rev_1).length, ((java.math.BigInteger)(n)).longValue())]));
    }
    public static void main(String[] args) {
        System.out.println(bottom_up_cut_rod(java.math.BigInteger.valueOf(4), ((java.math.BigInteger[])(prices))));
        System.out.println(bottom_up_cut_rod(java.math.BigInteger.valueOf(10), ((java.math.BigInteger[])(prices))));
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
