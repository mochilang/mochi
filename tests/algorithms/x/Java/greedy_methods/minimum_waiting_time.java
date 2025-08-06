public class Main {

    static int[] insertion_sort(int[] a) {
        int i = 1;
        while (i < a.length) {
            int key = a[i];
            int j = i - 1;
            while (j >= 0 && a[j] > key) {
a[j + 1] = a[j];
                j = j - 1;
            }
a[j + 1] = key;
            i = i + 1;
        }
        return a;
    }

    static int minimum_waiting_time(int[] queries) {
        int n = queries.length;
        if (n == 0 || n == 1) {
            return 0;
        }
        int[] sorted = ((int[])(insertion_sort(((int[])(queries)))));
        int total = 0;
        int i_1 = 0;
        while (i_1 < n) {
            total = total + sorted[i_1] * (n - i_1 - 1);
            i_1 = i_1 + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(minimum_waiting_time(((int[])(new int[]{3, 2, 1, 2, 6}))));
            System.out.println(minimum_waiting_time(((int[])(new int[]{3, 2, 1}))));
            System.out.println(minimum_waiting_time(((int[])(new int[]{1, 2, 3, 4}))));
            System.out.println(minimum_waiting_time(((int[])(new int[]{5, 5, 5, 5}))));
            System.out.println(minimum_waiting_time(((int[])(new int[]{}))));
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
