public class Main {

    static double abs_val(double num) {
        if (num < 0.0) {
            return -num;
        }
        return num;
    }

    static int abs_min(int[] x) {
        if (x.length == 0) {
            throw new RuntimeException(String.valueOf("abs_min() arg is an empty sequence"));
        }
        int j = x[0];
        int idx = 0;
        while (idx < x.length) {
            int i = x[idx];
            if (abs_val(((Number)(i)).doubleValue()) < abs_val(((Number)(j)).doubleValue())) {
                j = i;
            }
            idx = idx + 1;
        }
        return j;
    }

    static int abs_max(int[] x) {
        if (x.length == 0) {
            throw new RuntimeException(String.valueOf("abs_max() arg is an empty sequence"));
        }
        int j_1 = x[0];
        int idx_1 = 0;
        while (idx_1 < x.length) {
            int i_1 = x[idx_1];
            if (abs_val(((Number)(i_1)).doubleValue()) > abs_val(((Number)(j_1)).doubleValue())) {
                j_1 = i_1;
            }
            idx_1 = idx_1 + 1;
        }
        return j_1;
    }

    static int abs_max_sort(int[] x) {
        if (x.length == 0) {
            throw new RuntimeException(String.valueOf("abs_max_sort() arg is an empty sequence"));
        }
        int[] arr = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < x.length) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(x[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        int n = arr.length;
        int a = 0;
        while (a < n) {
            int b = 0;
            while (b < n - a - 1) {
                if (abs_val(((Number)(arr[b])).doubleValue()) > abs_val(((Number)(arr[b + 1])).doubleValue())) {
                    int temp = arr[b];
arr[b] = arr[b + 1];
arr[b + 1] = temp;
                }
                b = b + 1;
            }
            a = a + 1;
        }
        return arr[n - 1];
    }

    static void test_abs_val() {
        if (abs_val(0.0) != 0.0) {
            throw new RuntimeException(String.valueOf("abs_val(0) failed"));
        }
        if (abs_val(34.0) != 34.0) {
            throw new RuntimeException(String.valueOf("abs_val(34) failed"));
        }
        if (abs_val(-100000000000.0) != 100000000000.0) {
            throw new RuntimeException(String.valueOf("abs_val large failed"));
        }
        int[] a_1 = ((int[])(new int[]{-3, -1, 2, -11}));
        if (abs_max(((int[])(a_1))) != (-11)) {
            throw new RuntimeException(String.valueOf("abs_max failed"));
        }
        if (abs_max_sort(((int[])(a_1))) != (-11)) {
            throw new RuntimeException(String.valueOf("abs_max_sort failed"));
        }
        if (abs_min(((int[])(a_1))) != (-1)) {
            throw new RuntimeException(String.valueOf("abs_min failed"));
        }
    }

    static void main() {
        test_abs_val();
        System.out.println(abs_val(-34.0));
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
}
