public class Main {

    static int fib(int n) {
        if (n < 2) {
            return n;
        }
        int a = 0;
        int b = 1;
        int i = 1;
        while (i < n) {
            int t = a + b;
            a = b;
            b = t;
            i = i + 1;
        }
        return b;
    }

    static void main() {
        for (int i : new int[]{-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) {
            if (i < 0) {
                System.out.println("fib(" + _p(i) + ") returned error: negative n is forbidden");
            } else {
                System.out.println("fib(" + _p(i) + ") = " + _p(fib(i)));
            }
        }
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
