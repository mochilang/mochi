public class Main {

    static void main() {
        int row = 3;
        int col = 4;
        int[][] a = new int[][]{};
        int i = 0;
        while (i < row) {
            int[] rowArr = new int[]{};
            int j = 0;
            while (j < col) {
                rowArr = java.util.stream.IntStream.concat(java.util.Arrays.stream(rowArr), java.util.stream.IntStream.of(0)).toArray();
                j = j + 1;
            }
            a = appendObj(a, rowArr);
            i = i + 1;
        }
        System.out.println("a[0][0] = " + String.valueOf(a[0][0]));
a[((Number)((row - 1))).intValue()][((Number)((col - 1))).intValue()] = 7;
        System.out.println("a[" + String.valueOf(row - 1) + "][" + String.valueOf(col - 1) + "] = " + String.valueOf(a[((Number)((row - 1))).intValue()][((Number)((col - 1))).intValue()]));
        a = null;
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
}
