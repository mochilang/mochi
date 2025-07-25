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
        for (var i : new int[]{-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) {
            if (i < 0) {
                System.out.println(String.valueOf("fib(" + String.valueOf(i)) + ") returned error: negative n is forbidden");
            } else {
                System.out.println(String.valueOf(String.valueOf("fib(" + String.valueOf(i)) + ") = ") + String.valueOf(fib(i)));
            }
        }
    }
    public static void main(String[] args) {
        main();
    }
}
