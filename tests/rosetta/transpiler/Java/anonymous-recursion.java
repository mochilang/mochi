public class Main {

    static int fib(int n) {
        if (n < 2) {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }

    static void main() {
        int i = -1;
        while (i <= 10) {
            if (i < 0) {
                System.out.println("fib(" + _p(i) + ") returned error: negative n is forbidden");
            } else {
                System.out.println("fib(" + _p(i) + ") = " + _p(fib(i)));
            }
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
