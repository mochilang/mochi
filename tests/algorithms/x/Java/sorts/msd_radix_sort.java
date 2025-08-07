public class Main {
    static int[] ex1 = new int[0];
    static int[] sorted1 = new int[0];
    static int[] ex2 = new int[0];
    static int[] sorted2 = new int[0];
    static int[] ex3 = new int[0];
    static int[] sorted3 = new int[0];
    static int[] ex4 = new int[0];
    static int[] sorted4 = new int[0];

    static int get_bit_length(int n) {
        if (n == 0) {
            return 1;
        }
        int length = 0;
        int num = n;
        while (num > 0) {
            length = length + 1;
            num = Math.floorDiv(num, 2);
        }
        return length;
    }

    static int max_bit_length(int[] nums) {
        int i = 0;
        int max_len = 0;
        while (i < nums.length) {
            int l = get_bit_length(nums[i]);
            if (l > max_len) {
                max_len = l;
            }
            i = i + 1;
        }
        return max_len;
    }

    static int get_bit(int num, int pos) {
        int n = num;
        int i_1 = 0;
        while (i_1 < pos) {
            n = Math.floorDiv(n, 2);
            i_1 = i_1 + 1;
        }
        return Math.floorMod(n, 2);
    }

    static int[] _msd_radix_sort(int[] nums, int bit_position) {
        if (bit_position == 0 || nums.length <= 1) {
            return nums;
        }
        int[] zeros = ((int[])(new int[]{}));
        int[] ones = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < nums.length) {
            int num_1 = nums[i_2];
            if (get_bit(num_1, bit_position - 1) == 1) {
                ones = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ones), java.util.stream.IntStream.of(num_1)).toArray()));
            } else {
                zeros = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(zeros), java.util.stream.IntStream.of(num_1)).toArray()));
            }
            i_2 = i_2 + 1;
        }
        zeros = ((int[])(_msd_radix_sort(((int[])(zeros)), bit_position - 1)));
        ones = ((int[])(_msd_radix_sort(((int[])(ones)), bit_position - 1)));
        int[] res = ((int[])(zeros));
        i_2 = 0;
        while (i_2 < ones.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(ones[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        return res;
    }

    static int[] msd_radix_sort(int[] nums) {
        if (nums.length == 0) {
            return new int[]{};
        }
        int i_3 = 0;
        while (i_3 < nums.length) {
            if (nums[i_3] < 0) {
                throw new RuntimeException(String.valueOf("All numbers must be positive"));
            }
            i_3 = i_3 + 1;
        }
        int bits = max_bit_length(((int[])(nums)));
        int[] result = ((int[])(_msd_radix_sort(((int[])(nums)), bits)));
        return result;
    }

    static int[] msd_radix_sort_inplace(int[] nums) {
        return msd_radix_sort(((int[])(nums)));
    }
    public static void main(String[] args) {
        ex1 = ((int[])(new int[]{40, 12, 1, 100, 4}));
        sorted1 = ((int[])(msd_radix_sort(((int[])(ex1)))));
        System.out.println(_p(sorted1));
        ex2 = ((int[])(new int[]{}));
        sorted2 = ((int[])(msd_radix_sort(((int[])(ex2)))));
        System.out.println(_p(sorted2));
        ex3 = ((int[])(new int[]{123, 345, 123, 80}));
        sorted3 = ((int[])(msd_radix_sort(((int[])(ex3)))));
        System.out.println(_p(sorted3));
        ex4 = ((int[])(new int[]{1209, 834598, 1, 540402, 45}));
        sorted4 = ((int[])(msd_radix_sort(((int[])(ex4)))));
        System.out.println(_p(sorted4));
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
