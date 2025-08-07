public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int factorial_digit_sum(int num) {
        int factorial = 1;
        int i = 1;
        while (i <= num) {
            factorial = factorial * i;
            i = i + 1;
        }
        String s = _p(factorial);
        int result = 0;
        int j = 0;
        while (j < _runeLen(s)) {
            result = result + (s.substring(j, j+1));
            j = j + 1;
        }
        return result;
    }

    static void main() {
        System.out.println("Enter the Number: ");
        int n = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        System.out.println(_p(factorial_digit_sum(n)));
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
