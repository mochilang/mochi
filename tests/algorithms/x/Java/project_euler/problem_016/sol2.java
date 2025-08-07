public class Main {

    static int solution(int power) {
        int[] digits = ((int[])(new int[]{}));
        digits = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(1)).toArray()));
        int i = 0;
        while (i < power) {
            int carry = 0;
            int j = 0;
            while (j < digits.length) {
                int v = digits[j] * 2 + carry;
digits[j] = Math.floorMod(v, 10);
                carry = Math.floorDiv(v, 10);
                j = j + 1;
            }
            if (carry > 0) {
                digits = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(carry)).toArray()));
            }
            i = i + 1;
        }
        int sum = 0;
        int k = 0;
        while (k < digits.length) {
            sum = sum + digits[k];
            k = k + 1;
        }
        return sum;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(1000)));
        System.out.println(_p(solution(50)));
        System.out.println(_p(solution(20)));
        System.out.println(_p(solution(15)));
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
