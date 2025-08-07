public class Main {

    static boolean is_prime(int number) {
        if (1 < number && number < 4) {
            return true;
        } else         if (number < 2 || Math.floorMod(number, 2) == 0 || Math.floorMod(number, 3) == 0) {
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

    static int solution(int a_limit, int b_limit) {
        int longest_len = 0;
        int longest_a = 0;
        int longest_b = 0;
        int a = (-1 * a_limit) + 1;
        while (a < a_limit) {
            int b = 2;
            while (b < b_limit) {
                if (((Boolean)(is_prime(b)))) {
                    int count = 0;
                    int n = 0;
                    while (is_prime(n * n + a * n + b)) {
                        count = count + 1;
                        n = n + 1;
                    }
                    if (count > longest_len) {
                        longest_len = count;
                        longest_a = a;
                        longest_b = b;
                    }
                }
                b = b + 1;
            }
            a = a + 1;
        }
        return longest_a * longest_b;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(1000, 1000)));
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
