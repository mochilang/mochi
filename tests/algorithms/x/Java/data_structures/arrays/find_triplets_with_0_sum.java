public class Main {

    static int[] sort_triplet(int a, int b, int c) {
        int x = a;
        int y = b;
        int z = c;
        if (x > y) {
            int t = x;
            x = y;
            y = t;
        }
        if (y > z) {
            int t_1 = y;
            y = z;
            z = t_1;
        }
        if (x > y) {
            int t_2 = x;
            x = y;
            y = t_2;
        }
        return new int[]{x, y, z};
    }

    static boolean contains_triplet(int[][] arr, int[] target) {
        for (int i = 0; i < arr.length; i++) {
            int[] item = ((int[])(arr[i]));
            boolean same = true;
            for (int j = 0; j < target.length; j++) {
                if (item[j] != target[j]) {
                    same = false;
                    break;
                }
            }
            if (same) {
                return true;
            }
        }
        return false;
    }

    static boolean contains_int(int[] arr, int value) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == value) {
                return true;
            }
        }
        return false;
    }

    static int[][] find_triplets_with_0_sum(int[] nums) {
        int n = nums.length;
        int[][] result = ((int[][])(new int[][]{}));
        for (int i = 0; i < n; i++) {
            for (int j = (i + 1); j < n; j++) {
                for (int k = (j + 1); k < n; k++) {
                    int a = nums[i];
                    int b = nums[j];
                    int c = nums[k];
                    if (a + b + c == 0) {
                        int[] trip = ((int[])(sort_triplet(a, b, c)));
                        if (!(Boolean)contains_triplet(((int[][])(result)), ((int[])(trip)))) {
                            result = ((int[][])(appendObj(result, trip)));
                        }
                    }
                }
            }
        }
        return result;
    }

    static int[][] find_triplets_with_0_sum_hashing(int[] arr) {
        int target_sum = 0;
        int[][] output = ((int[][])(new int[][]{}));
        for (int i = 0; i < arr.length; i++) {
            int[] seen = ((int[])(new int[]{}));
            int current_sum = target_sum - arr[i];
            for (int j = (i + 1); j < arr.length; j++) {
                int other = arr[j];
                int required = current_sum - other;
                if (((Boolean)(contains_int(((int[])(seen)), required)))) {
                    int[] trip_1 = ((int[])(sort_triplet(arr[i], other, required)));
                    if (!(Boolean)contains_triplet(((int[][])(output)), ((int[])(trip_1)))) {
                        output = ((int[][])(appendObj(output, trip_1)));
                    }
                }
                seen = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(seen), java.util.stream.IntStream.of(other)).toArray()));
            }
        }
        return output;
    }
    public static void main(String[] args) {
        System.out.println(_p(find_triplets_with_0_sum(((int[])(new int[]{-1, 0, 1, 2, -1, -4})))));
        System.out.println(_p(find_triplets_with_0_sum(((int[])(new int[]{})))));
        System.out.println(_p(find_triplets_with_0_sum(((int[])(new int[]{0, 0, 0})))));
        System.out.println(_p(find_triplets_with_0_sum(((int[])(new int[]{1, 2, 3, 0, -1, -2, -3})))));
        System.out.println(_p(find_triplets_with_0_sum_hashing(((int[])(new int[]{-1, 0, 1, 2, -1, -4})))));
        System.out.println(_p(find_triplets_with_0_sum_hashing(((int[])(new int[]{})))));
        System.out.println(_p(find_triplets_with_0_sum_hashing(((int[])(new int[]{0, 0, 0})))));
        System.out.println(_p(find_triplets_with_0_sum_hashing(((int[])(new int[]{1, 2, 3, 0, -1, -2, -3})))));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
