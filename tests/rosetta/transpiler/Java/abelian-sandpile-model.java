public class Main {
    static int dim = 16;

    static int[][] newPile(int d) {
        int[][] b = new int[][]{};
        int y = 0;
        while (y < d) {
            int[] row = new int[]{};
            int x = 0;
            while (x < d) {
                row = java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray();
                x = x + 1;
            }
            b = appendObj(b, row);
            y = y + 1;
        }
        return b;
    }

    static int[][] handlePile(int[][] pile, int x, int y) {
        if (pile[y][x] >= 4) {
pile[y][x] = pile[y][x] - 4;
            if (y > 0) {
pile[y - 1][x] = pile[y - 1][x] + 1;
                if (pile[y - 1][x] >= 4) {
                    pile = handlePile(pile, x, y - 1);
                }
            }
            if (x > 0) {
pile[y][x - 1] = pile[y][x - 1] + 1;
                if (pile[y][x - 1] >= 4) {
                    pile = handlePile(pile, x - 1, y);
                }
            }
            if (y < dim - 1) {
pile[y + 1][x] = pile[y + 1][x] + 1;
                if (pile[y + 1][x] >= 4) {
                    pile = handlePile(pile, x, y + 1);
                }
            }
            if (x < dim - 1) {
pile[y][x + 1] = pile[y][x + 1] + 1;
                if (pile[y][x + 1] >= 4) {
                    pile = handlePile(pile, x + 1, y);
                }
            }
            pile = handlePile(pile, x, y);
        }
        return pile;
    }

    static void drawPile(int[][] pile, int d) {
        String[] chars = new String[]{" ", "░", "▓", "█"};
        int row = 0;
        while (row < d) {
            String line = "";
            int col = 0;
            while (col < d) {
                int v = pile[row][col];
                if (v > 3) {
                    v = 3;
                }
                line = line + chars[v];
                col = col + 1;
            }
            System.out.println(line);
            row = row + 1;
        }
    }

    static void main() {
        int[][] pile = newPile(16);
        int hdim = 7;
pile[hdim][hdim] = 16;
        pile = handlePile(pile, hdim, hdim);
        drawPile(pile, 16);
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
        return rt.totalMemory() - rt.freeMemory();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
