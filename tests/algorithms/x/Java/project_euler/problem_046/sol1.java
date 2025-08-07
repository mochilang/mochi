public class Main {

    static int int_sqrt(int n) {
        int r = 0;
        while ((r + 1) * (r + 1) <= n) {
            r = r + 1;
        }
        return r;
    }

    static boolean is_prime(int number) {
        if (1 < number && number < 4) {
            return true;
        }
        if (number < 2 || Math.floorMod(number, 2) == 0 || Math.floorMod(number, 3) == 0) {
            return false;
        }
        int i = 5;
        int limit = int_sqrt(number);
        while (i <= limit) {
            if (Math.floorMod(number, i) == 0 || Math.floorMod(number, (i + 2)) == 0) {
                return false;
            }
            i = i + 6;
        }
        return true;
    }

    static int[] compute_nums(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("n must be >= 0"));
        }
        int[] list_nums = ((int[])(new int[]{}));
        int num = 3;
        while (list_nums.length < n) {
            if (!(Boolean)is_prime(num)) {
                int i_1 = 0;
                boolean found = false;
                while (2 * i_1 * i_1 <= num) {
                    int rem = num - 2 * i_1 * i_1;
                    if (((Boolean)(is_prime(rem)))) {
                        found = true;
                        break;
                    }
                    i_1 = i_1 + 1;
                }
                if (!found) {
                    list_nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(list_nums), java.util.stream.IntStream.of(num)).toArray()));
                }
            }
            num = num + 2;
        }
        return list_nums;
    }

    static int solution() {
        return compute_nums(1)[0];
    }
    public static void main(String[] args) {
        System.out.println(_p(solution()));
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
