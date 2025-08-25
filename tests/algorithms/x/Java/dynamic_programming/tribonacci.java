public class Main {

    static java.math.BigInteger[] tribonacci(java.math.BigInteger num) {
        java.math.BigInteger[] dp = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(num) < 0) {
            if (i_1.compareTo(java.math.BigInteger.valueOf(0)) == 0 || i_1.compareTo(java.math.BigInteger.valueOf(1)) == 0) {
                dp = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            } else             if (i_1.compareTo(java.math.BigInteger.valueOf(2)) == 0) {
                dp = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1))).toArray(java.math.BigInteger[]::new)));
            } else {
                java.math.BigInteger t_1 = new java.math.BigInteger(String.valueOf(dp[_idx((dp).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(dp[_idx((dp).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(2)))).longValue())]).add(dp[_idx((dp).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(3)))).longValue())])));
                dp = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(t_1)))).toArray(java.math.BigInteger[]::new)));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(dp));
    }
    public static void main(String[] args) {
        System.out.println(java.util.Arrays.toString(tribonacci(java.math.BigInteger.valueOf(8))));
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
