public class Main {

    static java.math.BigInteger factorial(int n) {
        java.math.BigInteger r = java.math.BigInteger.valueOf(1);
        int i = 2;
        while (i <= n) {
            r = r.multiply((new java.math.BigInteger(String.valueOf(i))));
            i = i + 1;
        }
        return r;
    }
    public static void main(String[] args) {
        for (int i = 0; i < 11; i++) {
            System.out.println((String)(_p(i)) + " " + (String)(_p(factorial(i))));
        }
        System.out.println("100 " + (String)(_p(factorial(100))));
        System.out.println("800 " + (String)(_p(factorial(800))));
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
