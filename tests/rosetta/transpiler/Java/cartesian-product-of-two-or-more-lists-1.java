public class Main {

    static int[][] cart2(int[] a, int[] b) {
        int[][] p = new int[][]{};
        for (int x : a) {
            for (int y : b) {
                p = appendObj(p, new int[]{x, y});
            }
        }
        return p;
    }

    static String llStr(int[][] lst) {
        String s = "[";
        int i = 0;
        while (i < lst.length) {
            int[] row = lst[i];
            s = s + "[";
            int j = 0;
            while (j < row.length) {
                s = s + _p(_geti(row, j));
                if (j < row.length - 1) {
                    s = s + " ";
                }
                j = j + 1;
            }
            s = s + "]";
            if (i < lst.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static void main() {
        System.out.println(llStr(cart2(new int[]{1, 2}, new int[]{3, 4})));
        System.out.println(llStr(cart2(new int[]{3, 4}, new int[]{1, 2})));
        System.out.println(llStr(cart2(new int[]{1, 2}, new int[]{})));
        System.out.println(llStr(cart2(new int[]{}, new int[]{1, 2})));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
