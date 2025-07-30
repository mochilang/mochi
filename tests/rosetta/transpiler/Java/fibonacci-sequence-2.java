public class Main {
    static java.math.BigInteger fib(int n) {
        if (n < 2) {
            return new java.math.BigInteger(String.valueOf(n));
        }
        java.math.BigInteger a = java.math.BigInteger.valueOf(0);
        java.math.BigInteger b = java.math.BigInteger.valueOf(1);
        int i = n;
        i = i - 1;
        while (i > 0) {
            java.math.BigInteger tmp = a.add(b);
            a = b;
            b = new java.math.BigInteger(String.valueOf(tmp));
            i = i - 1;
        }
        return b;
    }
    public static void main(String[] args) {
    }
}
