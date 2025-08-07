public class Main {

    static int double_linear_search(int[] array, int search_item) {
        int start_ind = 0;
        int end_ind = array.length - 1;
        while (start_ind <= end_ind) {
            if (array[start_ind] == search_item) {
                return start_ind;
            }
            if (array[end_ind] == search_item) {
                return end_ind;
            }
            start_ind = start_ind + 1;
            end_ind = end_ind - 1;
        }
        return -1;
    }

    static void main() {
        int[] data = ((int[])(build_range(100)));
        System.out.println(_p(double_linear_search(((int[])(data)), 40)));
    }

    static int[] build_range(int n) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(i)).toArray()));
            i = i + 1;
        }
        return res;
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
