public class Main {

    static boolean contains(int[] lst, int val) {
        for (int x : lst) {
            if (x == val) {
                return true;
            }
        }
        return false;
    }

    static int solution(int numerator, int limit) {
        int the_digit = 1;
        int longest_len = 0;
        for (int d = numerator; d < limit; d++) {
            int[] remainders = ((int[])(new int[]{}));
            int rem = numerator;
            int count = 1;
            while (count <= limit) {
                if (((Boolean)(contains(((int[])(remainders)), rem)))) {
                    if (longest_len < remainders.length) {
                        longest_len = remainders.length;
                        the_digit = d;
                    }
                } else {
                    remainders = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(remainders), java.util.stream.IntStream.of(rem)).toArray()));
                    rem = Math.floorMod(rem * 10, d);
                }
                count = count + 1;
            }
        }
        return the_digit;
    }

    static void main() {
        System.out.println(_p(solution(1, 10)));
        System.out.println(_p(solution(10, 100)));
        System.out.println(_p(solution(10, 1000)));
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
