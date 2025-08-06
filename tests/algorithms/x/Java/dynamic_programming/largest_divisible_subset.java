public class Main {

    static int[] sort_list(int[] nums) {
        int[] arr = ((int[])(nums));
        int i = 1;
        while (i < arr.length) {
            int key = arr[i];
            int j = i - 1;
            while (j >= 0 && arr[j] > key) {
arr[j + 1] = arr[j];
                j = j - 1;
            }
arr[j + 1] = key;
            i = i + 1;
        }
        return arr;
    }

    static int[] largest_divisible_subset(int[] items) {
        if (items.length == 0) {
            return new int[]{};
        }
        int[] nums = ((int[])(sort_list(((int[])(items)))));
        int n = nums.length;
        int[] memo = ((int[])(new int[]{}));
        int[] prev = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            memo = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(memo), java.util.stream.IntStream.of(1)).toArray()));
            prev = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prev), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < n) {
            int j_1 = 0;
            while (j_1 < i_1) {
                if ((nums[j_1] == 0 || Math.floorMod(nums[i_1], nums[j_1]) == 0) && memo[j_1] + 1 > memo[i_1]) {
memo[i_1] = memo[j_1] + 1;
prev[i_1] = j_1;
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        int ans = 0 - 1;
        int last_index = 0 - 1;
        i_1 = 0;
        while (i_1 < n) {
            if (memo[i_1] > ans) {
                ans = memo[i_1];
                last_index = i_1;
            }
            i_1 = i_1 + 1;
        }
        if (last_index == 0 - 1) {
            return new int[]{};
        }
        int[] result = ((int[])(new int[]{nums[last_index]}));
        while (prev[last_index] != last_index) {
            last_index = prev[last_index];
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(nums[last_index])).toArray()));
        }
        return result;
    }

    static void main() {
        int[] items = ((int[])(new int[]{1, 16, 7, 8, 4}));
        int[] subset = ((int[])(largest_divisible_subset(((int[])(items)))));
        System.out.println("The longest divisible subset of " + _p(items) + " is " + _p(subset) + ".");
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
}
