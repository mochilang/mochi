public class Main {
    static int precision;

    static int lin_search(int left, int right, int[] array, int target) {
        int i = left;
        while (i < right) {
            if (array[i] == target) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int ite_ternary_search(int[] array, int target) {
        int left = 0;
        int right = array.length - 1;
        while (left <= right) {
            if (right - left < precision) {
                int idx = lin_search(left, right + 1, ((int[])(array)), target);
                return idx;
            }
            int one_third = left + Math.floorDiv((right - left), 3);
            int two_third = right - Math.floorDiv((right - left), 3);
            if (array[one_third] == target) {
                return one_third;
            }
            if (array[two_third] == target) {
                return two_third;
            }
            if (target < array[one_third]) {
                right = one_third - 1;
            } else             if (array[two_third] < target) {
                left = two_third + 1;
            } else {
                left = one_third + 1;
                right = two_third - 1;
            }
        }
        return -1;
    }

    static int rec_ternary_search(int left, int right, int[] array, int target) {
        if (left <= right) {
            if (right - left < precision) {
                int idx_1 = lin_search(left, right + 1, ((int[])(array)), target);
                return idx_1;
            }
            int one_third_1 = left + Math.floorDiv((right - left), 3);
            int two_third_1 = right - Math.floorDiv((right - left), 3);
            if (array[one_third_1] == target) {
                return one_third_1;
            }
            if (array[two_third_1] == target) {
                return two_third_1;
            }
            if (target < array[one_third_1]) {
                return rec_ternary_search(left, one_third_1 - 1, ((int[])(array)), target);
            }
            if (array[two_third_1] < target) {
                return rec_ternary_search(two_third_1 + 1, right, ((int[])(array)), target);
            }
            return rec_ternary_search(one_third_1 + 1, two_third_1 - 1, ((int[])(array)), target);
        }
        return -1;
    }

    static void main() {
        int[] test_list = ((int[])(new int[]{0, 1, 2, 8, 13, 17, 19, 32, 42}));
        System.out.println(_p(ite_ternary_search(((int[])(test_list)), 3)));
        System.out.println(_p(ite_ternary_search(((int[])(test_list)), 13)));
        System.out.println(_p(rec_ternary_search(0, test_list.length - 1, ((int[])(test_list)), 3)));
        System.out.println(_p(rec_ternary_search(0, test_list.length - 1, ((int[])(test_list)), 13)));
    }
    public static void main(String[] args) {
        precision = 10;
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
