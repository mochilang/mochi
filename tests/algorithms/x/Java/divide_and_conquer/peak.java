public class Main {

    static int peak(int[] lst) {
        int low = 0;
        int high = lst.length - 1;
        while (low < high) {
            int mid = (low + high) / 2;
            if (lst[mid] < lst[mid + 1]) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return lst[low];
    }

    static void main() {
        System.out.println(_p(peak(((int[])(new int[]{1, 2, 3, 4, 5, 4, 3, 2, 1})))));
        System.out.println(_p(peak(((int[])(new int[]{1, 10, 9, 8, 7, 6, 5, 4})))));
        System.out.println(_p(peak(((int[])(new int[]{1, 9, 8, 7})))));
        System.out.println(_p(peak(((int[])(new int[]{1, 2, 3, 4, 5, 6, 7, 0})))));
        System.out.println(_p(peak(((int[])(new int[]{1, 2, 3, 4, 3, 2, 1, 0, -1, -2})))));
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
