public class Main {

    static int[] get_totients(int max_one) {
        int[] totients = ((int[])(new int[]{}));
        int i = 0;
        while (i < max_one) {
            totients = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(totients), java.util.stream.IntStream.of(i)).toArray()));
            i = i + 1;
        }
        i = 2;
        while (i < max_one) {
            if (totients[i] == i) {
                int x = i;
                while (x < max_one) {
totients[x] = totients[x] - Math.floorDiv(totients[x], i);
                    x = x + i;
                }
            }
            i = i + 1;
        }
        return totients;
    }

    static boolean has_same_digits(int num1, int num2) {
        int[] count1 = ((int[])(new int[]{}));
        int[] count2 = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < 10) {
            count1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(count1), java.util.stream.IntStream.of(0)).toArray()));
            count2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(count2), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        int n1 = num1;
        int n2 = num2;
        if (n1 == 0) {
count1[0] = count1[0] + 1;
        }
        if (n2 == 0) {
count2[0] = count2[0] + 1;
        }
        while (n1 > 0) {
            int d1 = Math.floorMod(n1, 10);
count1[d1] = count1[d1] + 1;
            n1 = Math.floorDiv(n1, 10);
        }
        while (n2 > 0) {
            int d2 = Math.floorMod(n2, 10);
count2[d2] = count2[d2] + 1;
            n2 = Math.floorDiv(n2, 10);
        }
        i_1 = 0;
        while (i_1 < 10) {
            if (count1[i_1] != count2[i_1]) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static int solution(int max_n) {
        int min_numerator = 1;
        int min_denominator = 0;
        int[] totients_1 = ((int[])(get_totients(max_n + 1)));
        int i_2 = 2;
        while (i_2 <= max_n) {
            int t = totients_1[i_2];
            if (i_2 * min_denominator < min_numerator * t && ((Boolean)(has_same_digits(i_2, t)))) {
                min_numerator = i_2;
                min_denominator = t;
            }
            i_2 = i_2 + 1;
        }
        return min_numerator;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(10000)));
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
