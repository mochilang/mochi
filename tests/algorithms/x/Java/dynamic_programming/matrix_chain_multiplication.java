public class Main {
    static java.math.BigInteger INF = java.math.BigInteger.valueOf(1000000000);

    static java.math.BigInteger matrix_chain_multiply(java.math.BigInteger[] arr) {
        if (new java.math.BigInteger(String.valueOf(arr.length)).compareTo(java.math.BigInteger.valueOf(2)) < 0) {
            return java.math.BigInteger.valueOf(0);
        }
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(arr.length));
        java.math.BigInteger[][] dp_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n_1) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(n_1) < 0) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(INF)))).toArray(java.math.BigInteger[]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            dp_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = new java.math.BigInteger(String.valueOf(n_1.subtract(java.math.BigInteger.valueOf(1))));
        while (i_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger j_3 = new java.math.BigInteger(String.valueOf(i_1));
            while (j_3.compareTo(n_1) < 0) {
                if (i_1.compareTo(j_3) == 0) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = java.math.BigInteger.valueOf(0);
                } else {
                    java.math.BigInteger k_1 = new java.math.BigInteger(String.valueOf(i_1));
                    while (k_1.compareTo(j_3) < 0) {
                        java.math.BigInteger cost_1 = new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())]).length, ((java.math.BigInteger)(k_1)).longValue())].add(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(k_1.add(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(k_1.add(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())]).add(arr[_idx((arr).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].multiply(arr[_idx((arr).length, ((java.math.BigInteger)(k_1)).longValue())]).multiply(arr[_idx((arr).length, ((java.math.BigInteger)(j_3)).longValue())]))));
                        if (cost_1.compareTo(dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())]) < 0) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_1)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = new java.math.BigInteger(String.valueOf(cost_1));
                        }
                        k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
                    }
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(dp_1[_idx((dp_1).length, 1L)][_idx((dp_1[_idx((dp_1).length, 1L)]).length, ((java.math.BigInteger)(n_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
    }
    public static void main(String[] args) {
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
