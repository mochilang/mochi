public class Main {
    static class HeapItem {
        int value;
        int list_idx;
        int elem_idx;
        HeapItem(int value, int list_idx, int elem_idx) {
            this.value = value;
            this.list_idx = list_idx;
            this.elem_idx = elem_idx;
        }
        HeapItem() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'list_idx': %s, 'elem_idx': %s}", String.valueOf(value), String.valueOf(list_idx), String.valueOf(elem_idx));
        }
    }

    static int INF;

    static int[] smallest_range(int[][] nums) {
        HeapItem[] heap = ((HeapItem[])(new HeapItem[]{}));
        int current_max = -INF;
        int i = 0;
        while (i < nums.length) {
            int first_val = nums[i][0];
            heap = ((HeapItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap), java.util.stream.Stream.of(new HeapItem(first_val, i, 0))).toArray(HeapItem[]::new)));
            if (first_val > current_max) {
                current_max = first_val;
            }
            i = i + 1;
        }
        int[] best = ((int[])(new int[]{-INF, INF}));
        while (heap.length > 0) {
            int min_idx = 0;
            int j = 1;
            while (j < heap.length) {
                HeapItem hj = heap[j];
                HeapItem hmin = heap[min_idx];
                if (hj.value < hmin.value) {
                    min_idx = j;
                }
                j = j + 1;
            }
            HeapItem item = heap[min_idx];
            HeapItem[] new_heap = ((HeapItem[])(new HeapItem[]{}));
            int k = 0;
            while (k < heap.length) {
                if (k != min_idx) {
                    new_heap = ((HeapItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_heap), java.util.stream.Stream.of(heap[k])).toArray(HeapItem[]::new)));
                }
                k = k + 1;
            }
            heap = ((HeapItem[])(new_heap));
            int current_min = item.value;
            if (current_max - current_min < best[1] - best[0]) {
                best = ((int[])(new int[]{current_min, current_max}));
            }
            if (item.elem_idx == nums[item.list_idx].length - 1) {
                break;
            }
            int next_val = nums[item.list_idx][item.elem_idx + 1];
            heap = ((HeapItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap), java.util.stream.Stream.of(new HeapItem(next_val, item.list_idx, item.elem_idx + 1))).toArray(HeapItem[]::new)));
            if (next_val > current_max) {
                current_max = next_val;
            }
        }
        return best;
    }

    static String list_to_string(int[] arr) {
        String s = "[";
        int i_1 = 0;
        while (i_1 < arr.length) {
            s = s + _p(_geti(arr, i_1));
            if (i_1 < arr.length - 1) {
                s = s + ", ";
            }
            i_1 = i_1 + 1;
        }
        return s + "]";
    }

    static void main() {
        int[] result1 = ((int[])(smallest_range(((int[][])(new int[][]{new int[]{4, 10, 15, 24, 26}, new int[]{0, 9, 12, 20}, new int[]{5, 18, 22, 30}})))));
        System.out.println(list_to_string(((int[])(result1))));
        int[] result2 = ((int[])(smallest_range(((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{1, 2, 3}, new int[]{1, 2, 3}})))));
        System.out.println(list_to_string(((int[])(result2))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000;
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
