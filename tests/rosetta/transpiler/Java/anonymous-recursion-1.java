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
        for (var n : new int[]{0, 1, 2, 3, 4, 5, 10, 40, -1}) {
            if (n < 0) {
                System.out.println("fib undefined for negative numbers");
            } else {
                System.out.println(String.valueOf(String.valueOf("fib " + String.valueOf(n)) + " = ") + String.valueOf(fib(n)));
            }
        }
    }
    public static void main(String[] args) {
        main();
    }
}
