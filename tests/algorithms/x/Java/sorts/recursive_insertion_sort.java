public class Main {

    static int[] insert_next(int[] collection, int index) {
        int[] arr = ((int[])(collection));
        if (index >= arr.length || arr[index - 1] <= arr[index]) {
            return arr;
        }
        int j = index - 1;
        int temp = arr[j];
arr[j] = arr[index];
arr[index] = temp;
        return insert_next(((int[])(arr)), index + 1);
    }

    static int[] rec_insertion_sort(int[] collection, int n) {
        int[] arr_1 = ((int[])(collection));
        if (arr_1.length <= 1 || n <= 1) {
            return arr_1;
        }
        arr_1 = ((int[])(insert_next(((int[])(arr_1)), n - 1)));
        return rec_insertion_sort(((int[])(arr_1)), n - 1);
    }

    static void test_rec_insertion_sort() {
        int[] col1 = ((int[])(new int[]{1, 2, 1}));
        col1 = ((int[])(rec_insertion_sort(((int[])(col1)), col1.length)));
        if (col1[0] != 1 || col1[1] != 1 || col1[2] != 2) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        int[] col2 = ((int[])(new int[]{2, 1, 0, -1, -2}));
        col2 = ((int[])(rec_insertion_sort(((int[])(col2)), col2.length)));
        if (col2[0] != (0 - 2)) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        if (col2[1] != (0 - 1)) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        if (col2[2] != 0) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        if (col2[3] != 1) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        if (col2[4] != 2) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        int[] col3 = ((int[])(new int[]{1}));
        col3 = ((int[])(rec_insertion_sort(((int[])(col3)), col3.length)));
        if (col3[0] != 1) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
    }

    static void main() {
        test_rec_insertion_sort();
        int[] numbers = ((int[])(new int[]{5, 3, 4, 1, 2}));
        numbers = ((int[])(rec_insertion_sort(((int[])(numbers)), numbers.length)));
        System.out.println(_p(numbers));
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
