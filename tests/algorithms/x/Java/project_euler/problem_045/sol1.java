public class Main {

    static double to_float(int x) {
        return x * 1.0;
    }

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static int floor(double x) {
        int n = 0;
        double y = x;
        while (y >= 1.0) {
            y = y - 1.0;
            n = n + 1;
        }
        return n;
    }

    static int hexagonal_num(int n) {
        return n * (2 * n - 1);
    }

    static boolean is_pentagonal(int n) {
        double root = sqrt(1.0 + 24.0 * to_float(n));
        double val = (1.0 + root) / 6.0;
        return val == to_float(floor(val));
    }

    static int solution(int start) {
        int idx = start;
        int num = hexagonal_num(idx);
        while (!(Boolean)is_pentagonal(num)) {
            idx = idx + 1;
            num = hexagonal_num(idx);
        }
        return num;
    }

    static void test_hexagonal_num() {
        if (hexagonal_num(143) != 40755) {
            throw new RuntimeException(String.valueOf("hexagonal_num(143) failed"));
        }
        if (hexagonal_num(21) != 861) {
            throw new RuntimeException(String.valueOf("hexagonal_num(21) failed"));
        }
        if (hexagonal_num(10) != 190) {
            throw new RuntimeException(String.valueOf("hexagonal_num(10) failed"));
        }
    }

    static void test_is_pentagonal() {
        if (!(Boolean)is_pentagonal(330)) {
            throw new RuntimeException(String.valueOf("330 should be pentagonal"));
        }
        if (((Boolean)(is_pentagonal(7683)))) {
            throw new RuntimeException(String.valueOf("7683 should not be pentagonal"));
        }
        if (!(Boolean)is_pentagonal(2380)) {
            throw new RuntimeException(String.valueOf("2380 should be pentagonal"));
        }
    }

    static void test_solution() {
        if (solution(144) != 1533776805) {
            throw new RuntimeException(String.valueOf("solution failed"));
        }
    }
    public static void main(String[] args) {
        test_hexagonal_num();
        test_is_pentagonal();
        test_solution();
        System.out.println(_p(solution(144)) + " = ");
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
