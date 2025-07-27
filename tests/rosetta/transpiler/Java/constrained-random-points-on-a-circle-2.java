public class Main {
    static int nPts = 100;
    static int rMin = 10;
    static int rMax = 15;
    static int span = rMax + 1 + rMax;
    static int[][] poss = new int[][]{};
    static int min2 = rMin * rMin;
    static int max2 = rMax * rMax;
    static int y = -rMax;
    static String[][] rows = new String[][]{};
    static int r = 0;
    static int u = 0;
    static java.util.Map<String,Boolean> seen = new java.util.LinkedHashMap<String, Boolean>();
    static int n = 0;
    static int i2 = 0;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (y <= rMax) {
                int x = -rMax;
                while (x <= rMax) {
                    int r2 = x * x + y * y;
                    if (r2 >= min2 && r2 <= max2) {
                        poss = appendObj(poss, new int[]{x, y});
                    }
                    x = x + 1;
                }
                y = y + 1;
            }
            System.out.println(String.valueOf(poss.length) + " possible points");
            while (r < span) {
                String[] row = new String[]{};
                int c = 0;
                while (c < span * 2) {
                    row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(" ")).toArray(String[]::new);
                    c = c + 1;
                }
                rows = appendObj(rows, row);
                r = r + 1;
            }
            while (n < nPts) {
                int i = Math.floorMod(_now(), poss.length);
                int x = poss[i][0];
                int yy = poss[i][1];
                int row = yy + rMax;
                int col = (x + rMax) * 2;
rows[row][col] = "*";
                String key = String.valueOf(row) + "," + String.valueOf(col);
                if (!((boolean)seen.getOrDefault(key, false))) {
seen.put(key, true);
                    u = u + 1;
                }
                n = n + 1;
            }
            while (i2 < span) {
                String line = "";
                int j = 0;
                while (j < span * 2) {
                    line = line + rows[i2][j];
                    j = j + 1;
                }
                System.out.println(line);
                i2 = i2 + 1;
            }
            System.out.println(String.valueOf(u) + " unique points");
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
