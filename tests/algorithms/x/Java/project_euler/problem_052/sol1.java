public class Main {

    static int[] digits_count(int n) {
        int[] counts = ((int[])(new int[]{}));
        int i = 0;
        while (i < 10) {
            counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int x = n;
        if (x == 0) {
counts[0] = counts[0] + 1;
        }
        while (x > 0) {
            int d = Math.floorMod(x, 10);
counts[d] = counts[d] + 1;
            x = Math.floorDiv(x, 10);
        }
        return counts;
    }

    static boolean equal_lists(int[] a, int[] b) {
        int i_1 = 0;
        while (i_1 < a.length) {
            if (a[i_1] != b[i_1]) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static int solution() {
        int i_2 = 1;
        while (true) {
            int[] c = ((int[])(digits_count(i_2)));
            if (((Boolean)(equal_lists(((int[])(c)), ((int[])(digits_count(2 * i_2)))))) && ((Boolean)(equal_lists(((int[])(c)), ((int[])(digits_count(3 * i_2)))))) && ((Boolean)(equal_lists(((int[])(c)), ((int[])(digits_count(4 * i_2)))))) && ((Boolean)(equal_lists(((int[])(c)), ((int[])(digits_count(5 * i_2)))))) && ((Boolean)(equal_lists(((int[])(c)), ((int[])(digits_count(6 * i_2))))))) {
                return i_2;
            }
            i_2 = i_2 + 1;
        }
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
