public class Main {
    static int width;
    static int height;
    static int iterations;
    static String[][] grid;
    static int y;
    static int seed;
    static int[][] vertices;
    static int px;
    static int py;
    static int i;

    static int[] randInt(int s, int n) {
        int next = Math.floorMod((s * 1664525 + 1013904223), 2147483647);
        return new int[]{next, Math.floorMod(next, n)};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            width = 60;
            height = ((Number)((((Number)(width)).doubleValue() * 0.86602540378))).intValue();
            iterations = 5000;
            grid = ((String[][])(new String[][]{}));
            y = 0;
            while (y < height) {
                String[] line = ((String[])(new String[]{}));
                int x = 0;
                while (x < width) {
                    line = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(line), java.util.stream.Stream.of(" ")).toArray(String[]::new)));
                    x = x + 1;
                }
                grid = ((String[][])(appendObj(grid, line)));
                y = y + 1;
            }
            seed = 1;
            vertices = ((int[][])(new int[][]{new int[]{0, height - 1}, new int[]{width - 1, height - 1}, new int[]{((Number)((width / 2))).intValue(), 0}}));
            px = ((Number)((width / 2))).intValue();
            py = ((Number)((height / 2))).intValue();
            i = 0;
            while (i < iterations) {
                int[] r = ((int[])(randInt(seed, 3)));
                seed = r[0];
                int idx = r[1];
                int[] v = ((int[])(vertices[idx]));
                px = ((Number)(((px + v[0]) / 2))).intValue();
                py = ((Number)(((py + v[1]) / 2))).intValue();
                if (px >= 0 && px < width && py >= 0 && py < height) {
grid[py][px] = "*";
                }
                i = i + 1;
            }
            y = 0;
            while (y < height) {
                String line_1 = "";
                int x_1 = 0;
                while (x_1 < width) {
                    line_1 = line_1 + grid[y][x_1];
                    x_1 = x_1 + 1;
                }
                System.out.println(line_1);
                y = y + 1;
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
