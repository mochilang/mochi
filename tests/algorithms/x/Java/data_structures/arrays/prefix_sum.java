public class Main {
    static class PrefixSum {
        int[] prefix_sum;
        PrefixSum(int[] prefix_sum) {
            this.prefix_sum = prefix_sum;
        }
        PrefixSum() {}
        @Override public String toString() {
            return String.format("{'prefix_sum': %s}", String.valueOf(prefix_sum));
        }
    }

    static PrefixSum ps;
    static PrefixSum ps2;

    static PrefixSum make_prefix_sum(int[] arr) {
        int[] prefix = ((int[])(new int[]{}));
        int running = 0;
        int i = 0;
        while (i < arr.length) {
            running = running + arr[i];
            prefix = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prefix), java.util.stream.IntStream.of(running)).toArray()));
            i = i + 1;
        }
        return new PrefixSum(prefix);
    }

    static int get_sum(PrefixSum ps, int start, int end) {
        int[] prefix_1 = ((int[])(ps.prefix_sum));
        if (prefix_1.length == 0) {
            throw new RuntimeException(String.valueOf("The array is empty."));
        }
        if (start < 0 || end >= prefix_1.length || start > end) {
            throw new RuntimeException(String.valueOf("Invalid range specified."));
        }
        if (start == 0) {
            return prefix_1[end];
        }
        return prefix_1[end] - prefix_1[start - 1];
    }

    static boolean contains_sum(PrefixSum ps, int target_sum) {
        int[] prefix_2 = ((int[])(ps.prefix_sum));
        int[] sums = ((int[])(new int[]{0}));
        int i_1 = 0;
        while (i_1 < prefix_2.length) {
            int sum_item = prefix_2[i_1];
            int j = 0;
            while (j < sums.length) {
                if (sums[j] == sum_item - target_sum) {
                    return true;
                }
                j = j + 1;
            }
            sums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sums), java.util.stream.IntStream.of(sum_item)).toArray()));
            i_1 = i_1 + 1;
        }
        return false;
    }
    public static void main(String[] args) {
        ps = make_prefix_sum(((int[])(new int[]{1, 2, 3})));
        System.out.println(_p(get_sum(ps, 0, 2)));
        System.out.println(_p(get_sum(ps, 1, 2)));
        System.out.println(_p(get_sum(ps, 2, 2)));
        System.out.println(_p(contains_sum(ps, 6)));
        System.out.println(_p(contains_sum(ps, 5)));
        System.out.println(_p(contains_sum(ps, 3)));
        System.out.println(_p(contains_sum(ps, 4)));
        System.out.println(_p(contains_sum(ps, 7)));
        ps2 = make_prefix_sum(((int[])(new int[]{1, -2, 3})));
        System.out.println(_p(contains_sum(ps2, 2)));
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
