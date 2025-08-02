public class Main {
    static String[][] g_2;

    static String[][] initGrid(int size) {
        String[][] g = new String[][]{};
        int y = 0;
        while (y < size) {
            String[] row = new String[]{};
            int x = 0;
            while (x < size) {
                row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(" ")).toArray(String[]::new);
                x = x + 1;
            }
            g = appendObj(g, row);
            y = y + 1;
        }
        return g;
    }

    static void set(String[][] g, int x, int y) {
        if (x >= 0 && x < g[0].length && y >= 0 && y < g.length) {
g[y][x] = "#";
        }
    }

    static String[][] circle(int r) {
        int size = r * 2 + 1;
        String[][] g_1 = initGrid(size);
        int x_1 = r;
        int y_1 = 0;
        int err = 1 - r;
        while (y_1 <= x_1) {
            set(g_1, r + x_1, r + y_1);
            set(g_1, r + y_1, r + x_1);
            set(g_1, r - x_1, r + y_1);
            set(g_1, r - y_1, r + x_1);
            set(g_1, r - x_1, r - y_1);
            set(g_1, r - y_1, r - x_1);
            set(g_1, r + x_1, r - y_1);
            set(g_1, r + y_1, r - x_1);
            y_1 = y_1 + 1;
            if (err < 0) {
                err = err + 2 * y_1 + 1;
            } else {
                x_1 = x_1 - 1;
                err = err + 2 * (y_1 - x_1) + 1;
            }
        }
        return g_1;
    }

    static String trimRight(String[] row) {
        int end = row.length;
        while (end > 0 && (row[end - 1].equals(" "))) {
            end = end - 1;
        }
        String s = "";
        int i = 0;
        while (i < end) {
            s = s + row[i];
            i = i + 1;
        }
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g_2 = circle(10);
            for (String[] row : g_2) {
                System.out.println(trimRight(row));
            }
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
