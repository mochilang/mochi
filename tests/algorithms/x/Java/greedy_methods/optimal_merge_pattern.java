public class Main {

    static int index_of_min(int[] xs) {
        int min_idx = 0;
        int i = 1;
        while (i < xs.length) {
            if (xs[i] < xs[min_idx]) {
                min_idx = i;
            }
            i = i + 1;
        }
        return min_idx;
    }

    static int[] remove_at(int[] xs, int idx) {
        int[] res = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 != idx) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int optimal_merge_pattern(int[] files) {
        int[] arr = ((int[])(files));
        int optimal_merge_cost = 0;
        while (arr.length > 1) {
            int temp = 0;
            int k = 0;
            while (k < 2) {
                int min_idx_1 = index_of_min(((int[])(arr)));
                temp = temp + arr[min_idx_1];
                arr = ((int[])(remove_at(((int[])(arr)), min_idx_1)));
                k = k + 1;
            }
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(temp)).toArray()));
            optimal_merge_cost = optimal_merge_cost + temp;
        }
        return optimal_merge_cost;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(optimal_merge_pattern(((int[])(new int[]{2, 3, 4}))));
            System.out.println(optimal_merge_pattern(((int[])(new int[]{5, 10, 20, 30, 30}))));
            System.out.println(optimal_merge_pattern(((int[])(new int[]{8, 8, 8, 8, 8}))));
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
}
