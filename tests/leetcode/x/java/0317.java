import java.util.*;

public class Main {
    static int shortestDistance(int[][] grid) {
        int rows = grid.length, cols = grid[0].length;
        int[][] dist = new int[rows][cols];
        int[][] reach = new int[rows][cols];
        int buildings = 0;
        for (int sr = 0; sr < rows; sr++) {
            for (int sc = 0; sc < cols; sc++) {
                if (grid[sr][sc] != 1) continue;
                buildings++;
                boolean[][] seen = new boolean[rows][cols];
                ArrayDeque<int[]> q = new ArrayDeque<>();
                q.add(new int[] {sr, sc, 0});
                seen[sr][sc] = true;
                while (!q.isEmpty()) {
                    int[] cur = q.poll();
                    int r = cur[0], c = cur[1], d = cur[2];
                    int[][] dirs = {{1,0},{-1,0},{0,1},{0,-1}};
                    for (int[] dir : dirs) {
                        int nr = r + dir[0], nc = c + dir[1];
                        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !seen[nr][nc]) {
                            seen[nr][nc] = true;
                            if (grid[nr][nc] == 0) {
                                dist[nr][nc] += d + 1;
                                reach[nr][nc]++;
                                q.add(new int[] {nr, nc, d + 1});
                            }
                        }
                    }
                }
            }
        }
        int ans = -1;
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid[r][c] == 0 && reach[r][c] == buildings) {
                    if (ans == -1 || dist[r][c] < ans) ans = dist[r][c];
                }
            }
        }
        return ans;
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        if (!sc.hasNextInt()) return;
        int t = sc.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int r = sc.nextInt(), c = sc.nextInt();
            int[][] grid = new int[r][c];
            for (int i = 0; i < r; i++)
                for (int j = 0; j < c; j++)
                    grid[i][j] = sc.nextInt();
            if (tc > 0) out.append("\n\n");
            out.append(shortestDistance(grid));
        }
        System.out.print(out);
    }
}
