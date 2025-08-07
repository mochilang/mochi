public class Main {

    static boolean is_prime(int number) {
        if (1 < number && number < 4) {
            return true;
        }
        if (number < 2 || Math.floorMod(number, 2) == 0 || Math.floorMod(number, 3) == 0) {
            return false;
        }
        int i = 5;
        while (i * i <= number) {
            if (Math.floorMod(number, i) == 0 || Math.floorMod(number, (i + 2)) == 0) {
                return false;
            }
            i = i + 6;
        }
        return true;
    }

    static int solution(double ratio) {
        int j = 3;
        int primes = 3;
        while ((((Number)(primes)).doubleValue()) / (2 * j - 1) >= ratio) {
            int i_1 = j * j + j + 1;
            int limit = (j + 2) * (j + 2);
            int step = j + 1;
            while (i_1 < limit) {
                if (((Boolean)(is_prime(i_1)))) {
                    primes = primes + 1;
                }
                i_1 = i_1 + step;
            }
            j = j + 2;
        }
        return j;
    }

    static void test_solution() {
        if (solution(0.5) != 11) {
            throw new RuntimeException(String.valueOf("solution 0.5 failed"));
        }
        if (solution(0.2) != 309) {
            throw new RuntimeException(String.valueOf("solution 0.2 failed"));
        }
        if (solution(0.111) != 11317) {
            throw new RuntimeException(String.valueOf("solution 0.111 failed"));
        }
    }

    static void main() {
        test_solution();
        System.out.println(_p(solution(0.1)));
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
