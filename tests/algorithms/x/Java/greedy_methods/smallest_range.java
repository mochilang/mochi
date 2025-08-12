public class Main {
    static class HeapItem {
        long value;
        long list_idx;
        long elem_idx;
        HeapItem(long value, long list_idx, long elem_idx) {
            this.value = value;
            this.list_idx = list_idx;
            this.elem_idx = elem_idx;
        }
        HeapItem() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'list_idx': %s, 'elem_idx': %s}", String.valueOf(value), String.valueOf(list_idx), String.valueOf(elem_idx));
        }
    }

    static long INF;

    static long[] smallest_range(long[][] nums) {
        HeapItem[] heap = ((HeapItem[])(new HeapItem[]{}));
        long current_max_1 = -INF;
        long i_1 = 0L;
        while (i_1 < nums.length) {
            long first_val_1 = nums[(int)((long)(i_1))][(int)((long)(0))];
            heap = ((HeapItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap), java.util.stream.Stream.of(new HeapItem(first_val_1, i_1, 0))).toArray(HeapItem[]::new)));
            if (first_val_1 > current_max_1) {
                current_max_1 = first_val_1;
            }
            i_1 = i_1 + 1;
        }
        long[] best_1 = ((long[])(new long[]{-INF, INF}));
        while (heap.length > 0) {
            long min_idx_1 = 0L;
            long j_1 = 1L;
            while (j_1 < heap.length) {
                HeapItem hj_1 = heap[(int)((long)(j_1))];
                HeapItem hmin_1 = heap[(int)((long)(min_idx_1))];
                if (hj_1.value < hmin_1.value) {
                    min_idx_1 = j_1;
                }
                j_1 = j_1 + 1;
            }
            HeapItem item_1 = heap[(int)((long)(min_idx_1))];
            HeapItem[] new_heap_1 = ((HeapItem[])(new HeapItem[]{}));
            long k_1 = 0L;
            while (k_1 < heap.length) {
                if (k_1 != min_idx_1) {
                    new_heap_1 = ((HeapItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_heap_1), java.util.stream.Stream.of(heap[(int)((long)(k_1))])).toArray(HeapItem[]::new)));
                }
                k_1 = k_1 + 1;
            }
            heap = ((HeapItem[])(new_heap_1));
            long current_min_1 = item_1.value;
            if (current_max_1 - current_min_1 < best_1[(int)((long)(1))] - best_1[(int)((long)(0))]) {
                best_1 = ((long[])(new long[]{current_min_1, current_max_1}));
            }
            if (item_1.elem_idx == nums[(int)((long)(item_1.list_idx))].length - 1) {
                break;
            }
            long next_val_1 = nums[(int)((long)(item_1.list_idx))][(int)((long)(item_1.elem_idx + 1))];
            heap = ((HeapItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap), java.util.stream.Stream.of(new HeapItem(next_val_1, item_1.list_idx, item_1.elem_idx + 1))).toArray(HeapItem[]::new)));
            if (next_val_1 > current_max_1) {
                current_max_1 = next_val_1;
            }
        }
        return best_1;
    }

    static String list_to_string(long[] arr) {
        String s = "[";
        long i_3 = 0L;
        while (i_3 < arr.length) {
            s = s + _p(_geti(arr, ((Number)(i_3)).intValue()));
            if (i_3 < arr.length - 1) {
                s = s + ", ";
            }
            i_3 = i_3 + 1;
        }
        return s + "]";
    }

    static void main() {
        long[] result1 = ((long[])(smallest_range(((long[][])(new long[][]{new long[]{4, 10, 15, 24, 26}, new long[]{0, 9, 12, 20}, new long[]{5, 18, 22, 30}})))));
        System.out.println(list_to_string(((long[])(result1))));
        long[] result2_1 = ((long[])(smallest_range(((long[][])(new long[][]{new long[]{1, 2, 3}, new long[]{1, 2, 3}, new long[]{1, 2, 3}})))));
        System.out.println(list_to_string(((long[])(result2_1))));
    }
    public static void main(String[] args) {
        INF = 1000000000;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
