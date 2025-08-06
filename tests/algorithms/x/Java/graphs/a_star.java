public class Main {
    static int[][] DIRECTIONS;
    static class SearchResult {
        int[][] path;
        int[][] action;
        SearchResult(int[][] path, int[][] action) {
            this.path = path;
            this.action = action;
        }
        SearchResult() {}
        @Override public String toString() {
            return String.format("{'path': %s, 'action': %s}", String.valueOf(path), String.valueOf(action));
        }
    }


    static int iabs(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static SearchResult search(int[][] grid, int[] init, int[] goal, int cost, int[][] heuristic) {
        int[][] closed = ((int[][])(new int[][]{}));
        int r = 0;
        while (r < grid.length) {
            int[] row = ((int[])(new int[]{}));
            int c = 0;
            while (c < grid[0].length) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                c = c + 1;
            }
            closed = ((int[][])(appendObj(closed, row)));
            r = r + 1;
        }
closed[init[0]][init[1]] = 1;
        int[][] action = ((int[][])(new int[][]{}));
        r = 0;
        while (r < grid.length) {
            int[] row_1 = ((int[])(new int[]{}));
            int c_1 = 0;
            while (c_1 < grid[0].length) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(0)).toArray()));
                c_1 = c_1 + 1;
            }
            action = ((int[][])(appendObj(action, row_1)));
            r = r + 1;
        }
        int x = init[0];
        int y = init[1];
        int g = 0;
        int f = g + heuristic[x][y];
        int[][] cell = ((int[][])(new int[][]{new int[]{f, g, x, y}}));
        boolean found = false;
        boolean resign = false;
        while ((!found) && (!resign)) {
            if (cell.length == 0) {
                throw new RuntimeException(String.valueOf("Algorithm is unable to find solution"));
            } else {
                int best_i = 0;
                int best_f = cell[0][0];
                int i = 1;
                while (i < cell.length) {
                    if (cell[i][0] < best_f) {
                        best_f = cell[i][0];
                        best_i = i;
                    }
                    i = i + 1;
                }
                int[] next_cell = ((int[])(cell[best_i]));
                int[][] new_cell = ((int[][])(new int[][]{}));
                i = 0;
                while (i < cell.length) {
                    if (i != best_i) {
                        new_cell = ((int[][])(appendObj(new_cell, cell[i])));
                    }
                    i = i + 1;
                }
                cell = ((int[][])(new_cell));
                x = next_cell[2];
                y = next_cell[3];
                g = next_cell[1];
                if (x == goal[0] && y == goal[1]) {
                    found = true;
                } else {
                    int d = 0;
                    while (d < DIRECTIONS.length) {
                        int x2 = x + DIRECTIONS[d][0];
                        int y2 = y + DIRECTIONS[d][1];
                        if (x2 >= 0 && x2 < grid.length && y2 >= 0 && y2 < grid[0].length && closed[x2][y2] == 0 && grid[x2][y2] == 0) {
                            int g2 = g + cost;
                            int f2 = g2 + heuristic[x2][y2];
                            cell = ((int[][])(appendObj(cell, new int[]{f2, g2, x2, y2})));
closed[x2][y2] = 1;
action[x2][y2] = d;
                        }
                        d = d + 1;
                    }
                }
            }
        }
        int[][] invpath = ((int[][])(new int[][]{}));
        x = goal[0];
        y = goal[1];
        invpath = ((int[][])(appendObj(invpath, new int[]{x, y})));
        while (x != init[0] || y != init[1]) {
            int dir = action[x][y];
            int x2_1 = x - DIRECTIONS[dir][0];
            int y2_1 = y - DIRECTIONS[dir][1];
            x = x2_1;
            y = y2_1;
            invpath = ((int[][])(appendObj(invpath, new int[]{x, y})));
        }
        int[][] path = ((int[][])(new int[][]{}));
        int idx = invpath.length - 1;
        while (idx >= 0) {
            path = ((int[][])(appendObj(path, invpath[idx])));
            idx = idx - 1;
        }
        return new SearchResult(path, action);
    }

    static void main() {
        int[][] grid = ((int[][])(new int[][]{new int[]{0, 1, 0, 0, 0, 0}, new int[]{0, 1, 0, 0, 0, 0}, new int[]{0, 1, 0, 0, 0, 0}, new int[]{0, 1, 0, 0, 1, 0}, new int[]{0, 0, 0, 0, 1, 0}}));
        int[] init = ((int[])(new int[]{0, 0}));
        int[] goal = ((int[])(new int[]{grid.length - 1, grid[0].length - 1}));
        int cost = 1;
        int[][] heuristic = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < grid.length) {
            int[] row_2 = ((int[])(new int[]{}));
            int j = 0;
            while (j < grid[0].length) {
                int h = iabs(i_1 - goal[0]) + iabs(j - goal[1]);
                if (grid[i_1][j] == 1) {
                    row_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(99)).toArray()));
                } else {
                    row_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(h)).toArray()));
                }
                j = j + 1;
            }
            heuristic = ((int[][])(appendObj(heuristic, row_2)));
            i_1 = i_1 + 1;
        }
        SearchResult result = search(((int[][])(grid)), ((int[])(init)), ((int[])(goal)), cost, ((int[][])(heuristic)));
        System.out.println("ACTION MAP");
        int rr = 0;
        while (rr < result.action.length) {
            System.out.println(java.util.Arrays.toString(result.action[rr]));
            rr = rr + 1;
        }
        int p = 0;
        while (p < result.path.length) {
            System.out.println(java.util.Arrays.toString(result.path[p]));
            p = p + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            DIRECTIONS = ((int[][])(new int[][]{new int[]{-1, 0}, new int[]{0, -1}, new int[]{1, 0}, new int[]{0, 1}}));
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
