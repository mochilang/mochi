public class Main {
    static int w;
    static int h;
    static int n;
    static int frost;
    static int[][] grid;
    static int y;
    static int a;

    static boolean inBounds(int x, int y) {
        return x >= 0 && x < w && y >= 0 && y < h;
    }

    static boolean hasNeighbor(int x, int y) {
        int dy = -1;
        while (dy <= 1) {
            int dx = -1;
            while (dx <= 1) {
                if (!(dx == 0 && dy == 0)) {
                    int nx = x + dx;
                    int ny = y + dy;
                    if (((Boolean)(inBounds(nx, ny))) && grid[ny][nx] == frost) {
                        return true;
                    }
                }
                dx = dx + 1;
            }
            dy = dy + 1;
        }
        return false;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            w = 400;
            h = 300;
            n = 15000;
            frost = 255;
            grid = new int[][]{};
            y = 0;
            while (y < h) {
                int[] row = new int[]{};
                int x = 0;
                while (x < w) {
                    row = java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray();
                    x = x + 1;
                }
                grid = appendObj(grid, row);
                y = y + 1;
            }
grid[h / 3][w / 3] = frost;
            a = 0;
            while (a < n) {
                int px = Math.floorMod(_now(), w);
                int py = Math.floorMod(_now(), h);
                if (grid[py][px] == frost) {
                    boolean lost = false;
                    while (true) {
                        px = px + (Math.floorMod(_now(), 3)) - 1;
                        py = py + (Math.floorMod(_now(), 3)) - 1;
                        if (!(Boolean)inBounds(px, py)) {
                            lost = true;
                            break;
                        }
                        if (grid[py][px] != frost) {
                            break;
                        }
                    }
                    if (lost) {
                        continue;
                    }
                } else {
                    boolean lost_1 = false;
                    while (!(Boolean)hasNeighbor(px, py)) {
                        px = px + (Math.floorMod(_now(), 3)) - 1;
                        py = py + (Math.floorMod(_now(), 3)) - 1;
                        if (!(Boolean)inBounds(px, py)) {
                            lost_1 = true;
                            break;
                        }
                    }
                    if (lost_1) {
                        continue;
                    }
                }
grid[py][px] = frost;
                a = a + 1;
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
