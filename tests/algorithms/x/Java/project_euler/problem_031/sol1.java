public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int one_pence() {
        return 1;
    }

    static int two_pence(int x) {
        if (x < 0) {
            return 0;
        }
        return two_pence(x - 2) + one_pence();
    }

    static int five_pence(int x) {
        if (x < 0) {
            return 0;
        }
        return five_pence(x - 5) + two_pence(x);
    }

    static int ten_pence(int x) {
        if (x < 0) {
            return 0;
        }
        return ten_pence(x - 10) + five_pence(x);
    }

    static int twenty_pence(int x) {
        if (x < 0) {
            return 0;
        }
        return twenty_pence(x - 20) + ten_pence(x);
    }

    static int fifty_pence(int x) {
        if (x < 0) {
            return 0;
        }
        return fifty_pence(x - 50) + twenty_pence(x);
    }

    static int one_pound(int x) {
        if (x < 0) {
            return 0;
        }
        return one_pound(x - 100) + fifty_pence(x);
    }

    static int two_pound(int x) {
        if (x < 0) {
            return 0;
        }
        return two_pound(x - 200) + one_pound(x);
    }

    static int solution(int n) {
        return two_pound(n);
    }

    static void main() {
        int n = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        System.out.println(_p(solution(n)));
    }
    public static void main(String[] args) {
        main();
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
