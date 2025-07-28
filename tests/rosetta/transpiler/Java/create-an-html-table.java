public class Main {

    static void main() {
        int[][] rows = new int[][]{};
        for (int i = 0; i < 4; i++) {
            rows = appendObj(rows, new int[]{i * 3, i * 3 + 1, i * 3 + 2});
        }
        System.out.println("<table>");
        System.out.println("    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>");
        int idx = 0;
        for (int[] row : rows) {
            System.out.println("    <tr><td>" + String.valueOf(idx) + "</td><td>" + String.valueOf(row[0]) + "</td><td>" + String.valueOf(row[1]) + "</td><td>" + String.valueOf(row[2]) + "</td></tr>");
            idx = idx + 1;
        }
        System.out.println("</table>");
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
