public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int fibonacci(int n) {
        if (n == 1) {
            return 0;
        }
        if (n == 2) {
            return 1;
        }
        int a = 0;
        int b = 1;
        int i = 2;
        while (i <= n) {
            int c = a + b;
            a = b;
            b = c;
            i = i + 1;
        }
        return b;
    }

    static int fibonacci_digits_index(int n) {
        int digits = 0;
        int index = 2;
        while (digits < n) {
            index = index + 1;
            int fib = fibonacci(index);
            digits = _runeLen(_p(fib));
        }
        return index;
    }

    static int solution(int n) {
        return fibonacci_digits_index(n);
    }

    static void main() {
        int n = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        System.out.println(solution(n));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
