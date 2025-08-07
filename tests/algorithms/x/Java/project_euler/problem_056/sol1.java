public class Main {

    static int pow_int(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static int digital_sum(int n) {
        String s = _p(n);
        int sum = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            sum = sum + (s.substring(i_1, i_1+1));
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static int solution(int a, int b) {
        int max_sum = 0;
        int base = 0;
        while (base < a) {
            int power = 0;
            while (power < b) {
                int value = pow_int(base, power);
                int ds = digital_sum(value);
                if (ds > max_sum) {
                    max_sum = ds;
                }
                power = power + 1;
            }
            base = base + 1;
        }
        return max_sum;
    }

    static void test_solution() {
        if (solution(10, 10) != 45) {
            throw new RuntimeException(String.valueOf("solution 10 10 failed"));
        }
        if (solution(100, 100) != 972) {
            throw new RuntimeException(String.valueOf("solution 100 100 failed"));
        }
        if (solution(100, 200) != 1872) {
            throw new RuntimeException(String.valueOf("solution 100 200 failed"));
        }
    }

    static void main() {
        test_solution();
        System.out.println(_p(solution(100, 100)));
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
