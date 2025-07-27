public class Main {
    static int nPts = 100;
    static int rMin = 10;
    static int rMax = 15;
    static int span = rMax + 1 + rMax;
    static String[][] rows = new String[][]{};
    static int r = 0;
    static int u = 0;
    static java.util.Map<String,Boolean> seen = new java.util.LinkedHashMap<String, Boolean>();
    static int min2 = rMin * rMin;
    static int max2 = rMax * rMax;
    static int n = 0;
    static int i = 0;

    public static void main(String[] args) {
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
            int x = Math.floorMod(_now(), span) - rMax;
            int y = Math.floorMod(_now(), span) - rMax;
            int rs = x * x + y * y;
            if (rs < min2 || rs > max2) {
                continue;
            }
            n = n + 1;
            int row = y + rMax;
            int col = (x + rMax) * 2;
rows[row][col] = "*";
            String key = String.valueOf(row) + "," + String.valueOf(col);
            if (!((boolean)seen.getOrDefault(key, false))) {
seen.put(key, true);
                u = u + 1;
            }
        }
        while (i < span) {
            String line = "";
            int j = 0;
            while (j < span * 2) {
                line = line + rows[i][j];
                j = j + 1;
            }
            System.out.println(line);
            i = i + 1;
        }
        System.out.println(String.valueOf(u) + " unique points");
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
