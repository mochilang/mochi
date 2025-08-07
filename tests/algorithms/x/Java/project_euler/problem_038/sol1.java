public class Main {

    static boolean is_9_pandigital(int n) {
        int[] digits = ((int[])(new int[]{}));
        int i = 0;
        while (i < 10) {
            digits = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int count = 0;
        int x = n;
        while (x > 0) {
            int digit = Math.floorMod(x, 10);
            if (digit == 0) {
                return false;
            }
            if (digits[digit] == 1) {
                return false;
            }
digits[digit] = 1;
            x = Math.floorDiv(x, 10);
            count = count + 1;
        }
        return count == 9 && digits[1] == 1 && digits[2] == 1 && digits[3] == 1 && digits[4] == 1 && digits[5] == 1 && digits[6] == 1 && digits[7] == 1 && digits[8] == 1 && digits[9] == 1;
    }

    static int solution() {
        int base_num = 9999;
        while (base_num >= 5000) {
            int candidate = 100002 * base_num;
            if (((Boolean)(is_9_pandigital(candidate)))) {
                return candidate;
            }
            base_num = base_num - 1;
        }
        base_num = 333;
        while (base_num >= 100) {
            int candidate_1 = 1002003 * base_num;
            if (((Boolean)(is_9_pandigital(candidate_1)))) {
                return candidate_1;
            }
            base_num = base_num - 1;
        }
        return 0;
    }
    public static void main(String[] args) {
        System.out.println("solution() = " + _p(solution()));
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
