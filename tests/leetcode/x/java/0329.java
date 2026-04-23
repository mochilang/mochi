import java.io.*;

class Main {
    static int[][] matrix, memo;
    static int rows, cols;
    static final int[] DIRS = {1, 0, -1, 0, 1};

    static int dfs(int r, int c) {
        if (memo[r][c] != 0) return memo[r][c];
        int best = 1;
        for (int k = 0; k < 4; k++) {
            int nr = r + DIRS[k], nc = c + DIRS[k + 1];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && matrix[nr][nc] > matrix[r][c]) {
                best = Math.max(best, 1 + dfs(nr, nc));
            }
        }
        return memo[r][c] = best;
    }

    static int longestIncreasingPath(int[][] m) {
        matrix = m; rows = m.length; cols = m[0].length; memo = new int[rows][cols];
        int ans = 0;
        for (int r = 0; r < rows; r++) for (int c = 0; c < cols; c++) ans = Math.max(ans, dfs(r, c));
        return ans;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner(System.in);
        Integer tt = fs.nextIntOrNull(); if (tt == null) return;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < tt; tc++) {
            int r = fs.nextInt(), c = fs.nextInt();
            int[][] m = new int[r][c];
            for (int i = 0; i < r; i++) for (int j = 0; j < c; j++) m[i][j] = fs.nextInt();
            if (tc > 0) out.append("\n\n");
            out.append(longestIncreasingPath(m));
        }
        System.out.print(out);
    }

    static class FastScanner {
        private final byte[] data; private int idx = 0;
        FastScanner(InputStream is) throws IOException { data = is.readAllBytes(); }
        Integer nextIntOrNull() { skip(); if (idx >= data.length) return null; return nextInt(); }
        int nextInt() { skip(); int v = 0; while (idx < data.length && data[idx] > ' ') v = v * 10 + data[idx++] - '0'; return v; }
        void skip() { while (idx < data.length && data[idx] <= ' ') idx++; }
    }
}
