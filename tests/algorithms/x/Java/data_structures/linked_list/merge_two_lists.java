public class Main {
    static class SortedLinkedList {
        int[] values;
        SortedLinkedList(int[] values) {
            this.values = values;
        }
        SortedLinkedList() {}
        @Override public String toString() {
            return String.format("{'values': %s}", String.valueOf(values));
        }
    }


    static int[] sort_list(int[] nums) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < nums.length) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(nums[i])).toArray()));
            i = i + 1;
        }
        int j = 0;
        while (j < arr.length) {
            int k = j + 1;
            while (k < arr.length) {
                if (arr[k] < arr[j]) {
                    int tmp = arr[j];
arr[j] = arr[k];
arr[k] = tmp;
                }
                k = k + 1;
            }
            j = j + 1;
        }
        return arr;
    }

    static SortedLinkedList make_sorted_linked_list(int[] ints) {
        return new SortedLinkedList(sort_list(((int[])(ints))));
    }

    static int len_sll(SortedLinkedList sll) {
        return sll.values.length;
    }

    static String str_sll(SortedLinkedList sll) {
        String res = "";
        int i_1 = 0;
        while (i_1 < sll.values.length) {
            res = res + _p(_geti(sll.values, i_1));
            if (i_1 + 1 < sll.values.length) {
                res = res + " -> ";
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static SortedLinkedList merge_lists(SortedLinkedList a, SortedLinkedList b) {
        int[] combined = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < a.values.length) {
            combined = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(combined), java.util.stream.IntStream.of(a.values[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        i_2 = 0;
        while (i_2 < b.values.length) {
            combined = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(combined), java.util.stream.IntStream.of(b.values[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        return make_sorted_linked_list(((int[])(combined)));
    }

    static void main() {
        int[] test_data_odd = ((int[])(new int[]{3, 9, -11, 0, 7, 5, 1, -1}));
        int[] test_data_even = ((int[])(new int[]{4, 6, 2, 0, 8, 10, 3, -2}));
        SortedLinkedList sll_one = make_sorted_linked_list(((int[])(test_data_odd)));
        SortedLinkedList sll_two = make_sorted_linked_list(((int[])(test_data_even)));
        SortedLinkedList merged = merge_lists(sll_one, sll_two);
        System.out.println(_p(len_sll(merged)));
        System.out.println(str_sll(merged));
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
