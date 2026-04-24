import java.io.*;
import java.util.*;

class Main {
    static class Robot {
        String[] grid;
        int r, c, dir = 0;
        HashSet<String> cleaned = new HashSet<>();
        static final int[] DR = {-1, 0, 1, 0};
        static final int[] DC = {0, 1, 0, -1};

        Robot(String[] grid, int sr, int sc) { this.grid = grid; this.r = sr; this.c = sc; }
        boolean move() {
            int nr = r + DR[dir], nc = c + DC[dir];
            if (nr < 0 || nr >= grid.length || nc < 0 || nc >= grid[0].length() || grid[nr].charAt(nc) != '1') return false;
            r = nr; c = nc; return true;
        }
        void turnRight() { dir = (dir + 1) % 4; }
        void clean() { cleaned.add(r + "," + c); }
    }

    static void goBack(Robot robot) {
        robot.turnRight(); robot.turnRight();
        robot.move();
        robot.turnRight(); robot.turnRight();
    }

    static void dfs(Robot robot, int x, int y, int dir, HashSet<String> vis) {
        vis.add(x + "," + y);
        robot.clean();
        int[] dr = {-1, 0, 1, 0}, dc = {0, 1, 0, -1};
        for (int i = 0; i < 4; i++) {
            int nd = (dir + i) % 4;
            int nx = x + dr[nd], ny = y + dc[nd];
            String key = nx + "," + ny;
            if (!vis.contains(key) && robot.move()) {
                dfs(robot, nx, ny, nd, vis);
                goBack(robot);
            }
            robot.turnRight();
        }
    }

    static int solve(String[] grid, int sr, int sc) {
        Robot robot = new Robot(grid, sr, sc);
        dfs(robot, 0, 0, 0, new HashSet<>());
        return robot.cleaned.size();
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int idx = 0, t = Integer.parseInt(v[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(v[idx++]);
            int m = Integer.parseInt(v[idx++]);
            int sr = Integer.parseInt(v[idx++]);
            int sc = Integer.parseInt(v[idx++]);
            String[] grid = new String[n];
            for (int i = 0; i < n; i++) grid[i] = v[idx++];
            if (tc > 0) out.append("\n\n");
            out.append(solve(grid, sr, sc));
        }
        System.out.print(out);
    }
}
