public class Main {
    static String[][][] grid = new String[1][][];

    static void flood(int x, int y, String repl) {
        String target = grid[0][y][x];
        if ((target.equals(repl))) {
            return;
        }
        java.util.function.BiConsumer<Integer,Integer>[] ff = new java.util.function.BiConsumer[1];
        ff[0] = (px, py) -> {
        if (px < 0 || py < 0 || py >= grid[0].length || px >= grid[0][0].length) {
            return;
        }
        if (!(grid[0][py][px].equals(target))) {
            return;
        }
grid[0][py][px] = repl;
        ff[0].accept(px - 1, py);
        ff[0].accept(px + 1, py);
        ff[0].accept(px, py - 1);
        ff[0].accept(px, py + 1);
};
        String target_0 = target;
        ff[0].accept(x, y);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid[0] = new String[][]{new String[]{".", ".", ".", ".", "."}, new String[]{".", "#", "#", "#", "."}, new String[]{".", "#", ".", "#", "."}, new String[]{".", "#", "#", "#", "."}, new String[]{".", ".", ".", ".", "."}};
            flood(2, 2, "o");
            for (String[] row : grid[0]) {
                String line = "";
                for (String ch : row) {
                    line = line + ch;
                }
                System.out.println(line);
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
}
