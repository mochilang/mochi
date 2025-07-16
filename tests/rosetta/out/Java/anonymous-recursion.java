// anonymous-recursion.mochi
public class AnonymousRecursion {
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
                System.out.println("fib(" + String.valueOf(i) + ") returned error: negative n is forbidden");
            }
            else {
                System.out.println("fib(" + String.valueOf(i) + ") = " + String.valueOf(fib(i)));
            }
            i = (int)(i + 1);
        }
    }
    public static void main(String[] args) {
    main();
    }
}
