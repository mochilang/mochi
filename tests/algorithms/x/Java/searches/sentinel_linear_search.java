public class Main {

    static int[] remove_last(int[] xs) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < xs.length - 1) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        return res;
    }

    static int sentinel_linear_search(int[] sequence, int target) {
        int[] seq = ((int[])(sequence));
        seq = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(seq), java.util.stream.IntStream.of(target)).toArray()));
        int index = 0;
        while (seq[index] != target) {
            index = index + 1;
        }
        seq = ((int[])(remove_last(((int[])(seq)))));
        if (index == seq.length) {
            return -1;
        }
        return index;
    }
    public static void main(String[] args) {
        System.out.println(_p(sentinel_linear_search(((int[])(new int[]{0, 5, 7, 10, 15})), 0)));
        System.out.println(_p(sentinel_linear_search(((int[])(new int[]{0, 5, 7, 10, 15})), 15)));
        System.out.println(_p(sentinel_linear_search(((int[])(new int[]{0, 5, 7, 10, 15})), 5)));
        System.out.println(_p(sentinel_linear_search(((int[])(new int[]{0, 5, 7, 10, 15})), 6)));
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
