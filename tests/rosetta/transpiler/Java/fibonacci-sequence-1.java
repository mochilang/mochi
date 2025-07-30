public class Main {
    static int fib(int a) {
        if (a < 2) {
            return a;
        }
        return fib(a - 1) + fib(a - 2);
    }
    public static void main(String[] args) {
    }
}
