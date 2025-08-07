public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int solution(int n) {
        int a = 0;
        int b = 1;
        int index = 1;
        while (_runeLen(_p(b)) < n) {
            int temp = a + b;
            a = b;
            b = temp;
            index = index + 1;
        }
        return index;
    }

    static void main() {
        int n = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        int ans = solution(n);
        System.out.println(_p(ans));
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
