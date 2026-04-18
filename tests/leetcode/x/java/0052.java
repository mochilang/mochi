import java.io.*;
import java.util.*;

public class Main {
    static List<List<String>> solveNQueens(int n) {
        boolean[] cols = new boolean[n];
        boolean[] d1 = new boolean[2 * n];
        boolean[] d2 = new boolean[2 * n];
        char[][] board = new char[n][n];
        for (int i = 0; i < n; i++) Arrays.fill(board[i], '.');
        List<List<String>> res = new ArrayList<>();
        dfs(0, n, cols, d1, d2, board, res);
        return res;
    }

    static void dfs(int r, int n, boolean[] cols, boolean[] d1, boolean[] d2, char[][] board, List<List<String>> res) {
        if (r == n) {
            List<String> sol = new ArrayList<>();
            for (int i = 0; i < n; i++) sol.add(new String(board[i]));
            res.add(sol);
            return;
        }
        for (int c = 0; c < n; c++) {
            int a = r + c, b = r - c + n - 1;
            if (cols[c] || d1[a] || d2[b]) continue;
            cols[c] = d1[a] = d2[b] = true;
            board[r][c] = 'Q';
            dfs(r + 1, n, cols, d1, d2, board, res);
            board[r][c] = '.';
            cols[c] = d1[a] = d2[b] = false;
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line.trim());
        if (lines.isEmpty() || lines.get(0).isEmpty()) return;
        int idx = 0, t = Integer.parseInt(lines.get(idx++));
        List<String> out = new ArrayList<>();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(lines.get(idx++));
            List<List<String>> sols = solveNQueens(n);
            out.add(String.valueOf(sols.size()));
        }
        System.out.print(String.join("\n", out));
    }
}
