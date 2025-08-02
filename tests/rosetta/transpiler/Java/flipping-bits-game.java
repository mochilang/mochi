public class Main {

    static int[] randInt(int seed, int n) {
        int next = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
        return new int[]{next, Math.floorMod(next, n)};
    }

    static Object[] newBoard(int n, int seed) {
        int[][] board = new int[][]{};
        int s = seed;
        int i = 0;
        while (i < n) {
            int[] row = new int[]{};
            int j = 0;
            while (j < n) {
                int[] r = randInt(s, 2);
                s = r[0];
                row = java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(r[1])).toArray();
                j = j + 1;
            }
            board = appendObj(board, row);
            i = i + 1;
        }
        return new Object[]{board, s};
    }

    static int[][] copyBoard(int[][] b) {
        int[][] nb = new int[][]{};
        int i_1 = 0;
        while (i_1 < b.length) {
            int[] row_1 = new int[]{};
            int j_1 = 0;
            while (j_1 < b[i_1].length) {
                row_1 = java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(b[i_1][j_1])).toArray();
                j_1 = j_1 + 1;
            }
            nb = appendObj(nb, row_1);
            i_1 = i_1 + 1;
        }
        return nb;
    }

    static int[][] flipRow(int[][] b, int r) {
        int j_2 = 0;
        while (j_2 < b[r].length) {
b[r][j_2] = 1 - b[r][j_2];
            j_2 = j_2 + 1;
        }
        return b;
    }

    static int[][] flipCol(int[][] b, int c) {
        int i_2 = 0;
        while (i_2 < b.length) {
b[i_2][c] = 1 - b[i_2][c];
            i_2 = i_2 + 1;
        }
        return b;
    }

    static boolean boardsEqual(int[][] a, int[][] b) {
        int i_3 = 0;
        while (i_3 < a.length) {
            int j_3 = 0;
            while (j_3 < a[i_3].length) {
                if (a[i_3][j_3] != b[i_3][j_3]) {
                    return false;
                }
                j_3 = j_3 + 1;
            }
            i_3 = i_3 + 1;
        }
        return true;
    }

    static Object[] shuffleBoard(int[][] b, int seed) {
        int s_1 = seed;
        int n = b.length;
        int k = 0;
        while (k < 2 * n) {
            int[] r_1 = randInt(s_1, n);
            s_1 = r_1[0];
            int idx = r_1[1];
            if (Math.floorMod(k, 2) == 0) {
                b = flipRow(b, idx);
            } else {
                b = flipCol(b, idx);
            }
            k = k + 1;
        }
        return new Object[]{b, s_1};
    }

    static java.util.Map<String,int[]> solve(int[][] board, int[][] target) {
        int n_1 = board.length;
        int[] row_2 = new int[]{};
        int[] col = new int[]{};
        int i_4 = 0;
        while (i_4 < n_1) {
            int diff = board[i_4][0] != target[i_4][0] ? 1 : 0;
            row_2 = java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(diff)).toArray();
            i_4 = i_4 + 1;
        }
        int j_4 = 0;
        while (j_4 < n_1) {
            int diff_1 = board[0][j_4] != target[0][j_4] ? 1 : 0;
            int val = Math.floorMod((diff_1 + row_2[0]), 2);
            col = java.util.stream.IntStream.concat(java.util.Arrays.stream(col), java.util.stream.IntStream.of(val)).toArray();
            j_4 = j_4 + 1;
        }
        return new java.util.LinkedHashMap<String, int[]>(java.util.Map.ofEntries(java.util.Map.entry("row", row_2), java.util.Map.entry("col", col)));
    }

    static Object[] applySolution(int[][] b, java.util.Map<String,int[]> sol) {
        int[][] board_1 = b;
        int moves = 0;
        int i_5 = 0;
        while (i_5 < ((int[])(sol).get("row")).length) {
            if (((int[])(sol).get("row"))[i_5] == 1) {
                board_1 = flipRow(board_1, i_5);
                moves = moves + 1;
            }
            i_5 = i_5 + 1;
        }
        int j_5 = 0;
        while (j_5 < ((int[])(sol).get("col")).length) {
            if (((int[])(sol).get("col"))[j_5] == 1) {
                board_1 = flipCol(board_1, j_5);
                moves = moves + 1;
            }
            j_5 = j_5 + 1;
        }
        return new Object[]{board_1, moves};
    }

    static void printBoard(int[][] b) {
        int i_6 = 0;
        while (i_6 < b.length) {
            String line = "";
            int j_6 = 0;
            while (j_6 < b[i_6].length) {
                line = line + _p(_geti(b[i_6], j_6));
                if (j_6 < b[i_6].length - 1) {
                    line = line + " ";
                }
                j_6 = j_6 + 1;
            }
            System.out.println(line);
            i_6 = i_6 + 1;
        }
    }

    static void main() {
        int n_2 = 3;
        int seed = 1;
        Object[] res = newBoard(n_2, seed);
        int[][] target = ((int[][])(res[0]));
        seed = ((int)(res[1]));
        int[][] board_2 = copyBoard(target);
        while (true) {
            Object[] sres = shuffleBoard(copyBoard(board_2), seed);
            board_2 = ((int[][])(sres[0]));
            seed = ((int)(sres[1]));
            if (!(Boolean)boardsEqual(board_2, target)) {
                break;
            }
        }
        System.out.println("Target:");
        printBoard(target);
        System.out.println("Board:");
        printBoard(board_2);
        java.util.Map<String,int[]> sol = solve(board_2, target);
        Object[] ares = applySolution(board_2, sol);
        board_2 = ((int[][])(ares[0]));
        int moves_1 = ((int)(ares[1]));
        System.out.println("Solved:");
        printBoard(board_2);
        System.out.println("Moves: " + _p(moves_1));
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
