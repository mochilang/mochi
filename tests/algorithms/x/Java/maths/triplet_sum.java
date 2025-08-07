public class Main {

    static int[] bubble_sort(int[] nums) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < nums.length) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(nums[i])).toArray()));
            i = i + 1;
        }
        int n = arr.length;
        int a = 0;
        while (a < n) {
            int b = 0;
            while (b < n - a - 1) {
                if (arr[b] > arr[b + 1]) {
                    int tmp = arr[b];
arr[b] = arr[b + 1];
arr[b + 1] = tmp;
                }
                b = b + 1;
            }
            a = a + 1;
        }
        return arr;
    }

    static int[] sort3(int[] xs) {
        int[] arr_1 = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            arr_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr_1), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        int n_1 = arr_1.length;
        int a_1 = 0;
        while (a_1 < n_1) {
            int b_1 = 0;
            while (b_1 < n_1 - a_1 - 1) {
                if (arr_1[b_1] > arr_1[b_1 + 1]) {
                    int tmp_1 = arr_1[b_1];
arr_1[b_1] = arr_1[b_1 + 1];
arr_1[b_1 + 1] = tmp_1;
                }
                b_1 = b_1 + 1;
            }
            a_1 = a_1 + 1;
        }
        return arr_1;
    }

    static int[] triplet_sum1(int[] arr, int target) {
        int i_2 = 0;
        while (i_2 < arr.length - 2) {
            int j = i_2 + 1;
            while (j < arr.length - 1) {
                int k = j + 1;
                while (k < arr.length) {
                    if (arr[i_2] + arr[j] + arr[k] == target) {
                        return sort3(((int[])(new int[]{arr[i_2], arr[j], arr[k]})));
                    }
                    k = k + 1;
                }
                j = j + 1;
            }
            i_2 = i_2 + 1;
        }
        return new int[]{0, 0, 0};
    }

    static int[] triplet_sum2(int[] arr, int target) {
        int[] sorted = ((int[])(bubble_sort(((int[])(arr)))));
        int n_2 = sorted.length;
        int i_3 = 0;
        while (i_3 < n_2 - 2) {
            int left = i_3 + 1;
            int right = n_2 - 1;
            while (left < right) {
                int s = sorted[i_3] + sorted[left] + sorted[right];
                if (s == target) {
                    return new int[]{sorted[i_3], sorted[left], sorted[right]};
                }
                if (s < target) {
                    left = left + 1;
                } else {
                    right = right - 1;
                }
            }
            i_3 = i_3 + 1;
        }
        return new int[]{0, 0, 0};
    }

    static boolean list_equal(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        int i_4 = 0;
        while (i_4 < a.length) {
            if (a[i_4] != b[i_4]) {
                return false;
            }
            i_4 = i_4 + 1;
        }
        return true;
    }

    static void test_triplet_sum() {
        int[] arr1 = ((int[])(new int[]{13, 29, 7, 23, 5}));
        if (!(Boolean)list_equal(((int[])(triplet_sum1(((int[])(arr1)), 35))), ((int[])(new int[]{5, 7, 23})))) {
            throw new RuntimeException(String.valueOf("ts1 case1 failed"));
        }
        if (!(Boolean)list_equal(((int[])(triplet_sum2(((int[])(arr1)), 35))), ((int[])(new int[]{5, 7, 23})))) {
            throw new RuntimeException(String.valueOf("ts2 case1 failed"));
        }
        int[] arr2 = ((int[])(new int[]{37, 9, 19, 50, 44}));
        if (!(Boolean)list_equal(((int[])(triplet_sum1(((int[])(arr2)), 65))), ((int[])(new int[]{9, 19, 37})))) {
            throw new RuntimeException(String.valueOf("ts1 case2 failed"));
        }
        if (!(Boolean)list_equal(((int[])(triplet_sum2(((int[])(arr2)), 65))), ((int[])(new int[]{9, 19, 37})))) {
            throw new RuntimeException(String.valueOf("ts2 case2 failed"));
        }
        int[] arr3 = ((int[])(new int[]{6, 47, 27, 1, 15}));
        if (!(Boolean)list_equal(((int[])(triplet_sum1(((int[])(arr3)), 11))), ((int[])(new int[]{0, 0, 0})))) {
            throw new RuntimeException(String.valueOf("ts1 case3 failed"));
        }
        if (!(Boolean)list_equal(((int[])(triplet_sum2(((int[])(arr3)), 11))), ((int[])(new int[]{0, 0, 0})))) {
            throw new RuntimeException(String.valueOf("ts2 case3 failed"));
        }
    }

    static void main() {
        test_triplet_sum();
        int[] sample = ((int[])(new int[]{13, 29, 7, 23, 5}));
        int[] res = ((int[])(triplet_sum2(((int[])(sample)), 35)));
        System.out.println(_p(_geti(res, 0)) + " " + _p(_geti(res, 1)) + " " + _p(_geti(res, 2)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
