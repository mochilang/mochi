public class Main {
    static int[] DIGIT_FACTORIAL;

    static int digit_factorial_sum(int number) {
        if (number < 0) {
            throw new RuntimeException(String.valueOf("Parameter number must be greater than or equal to 0"));
        }
        if (number == 0) {
            return DIGIT_FACTORIAL[0];
        }
        int n = number;
        int total = 0;
        while (n > 0) {
            int digit = Math.floorMod(n, 10);
            total = total + DIGIT_FACTORIAL[digit];
            n = Math.floorDiv(n, 10);
        }
        return total;
    }

    static int chain_len(int n, int limit) {
        java.util.Map<Integer,Boolean> seen = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int length = 0;
        int cur = n;
        while (((Number)((seen.containsKey(cur)))).intValue() == false && length <= limit) {
seen.put(cur, true);
            length = length + 1;
            cur = digit_factorial_sum(cur);
        }
        return length;
    }

    static int solution(int chain_length, int number_limit) {
        if (chain_length <= 0 || number_limit <= 0) {
            throw new RuntimeException(String.valueOf("Parameters chain_length and number_limit must be greater than 0"));
        }
        int count = 0;
        int start = 1;
        while (start < number_limit) {
            if (chain_len(start, chain_length) == chain_length) {
                count = count + 1;
            }
            start = start + 1;
        }
        return count;
    }
    public static void main(String[] args) {
        DIGIT_FACTORIAL = ((int[])(new int[]{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880}));
        System.out.println(_p(solution(60, 1000000)));
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
