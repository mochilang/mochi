import java.io.*;
import java.util.*;

class Main {
    static class State {
        int d, r, c;
        String p;
        State(int d, int r, int c, String p) { this.d = d; this.r = r; this.c = c; this.p = p; }
    }

    static String solve(int[][] maze, int[] ball, int[] hole) {
        int m = maze.length, n = maze[0].length;
        int[][] dist = new int[m][n];
        String[][] path = new String[m][n];
        for (int i = 0; i < m; i++) Arrays.fill(dist[i], Integer.MAX_VALUE);
        PriorityQueue<State> pq = new PriorityQueue<>((a, b) -> a.d != b.d ? a.d - b.d : a.p.compareTo(b.p));
        pq.offer(new State(0, ball[0], ball[1], ""));
        dist[ball[0]][ball[1]] = 0;
        path[ball[0]][ball[1]] = "";
        int[] dr = {1, 0, 0, -1}, dc = {0, -1, 1, 0};
        char[] ch = {'d', 'l', 'r', 'u'};
        while (!pq.isEmpty()) {
            State cur = pq.poll();
            if (cur.r == hole[0] && cur.c == hole[1]) return cur.p;
            if (cur.d != dist[cur.r][cur.c] || !cur.p.equals(path[cur.r][cur.c])) continue;
            for (int k = 0; k < 4; k++) {
                int nr = cur.r, nc = cur.c, nd = cur.d;
                while (nr + dr[k] >= 0 && nr + dr[k] < m && nc + dc[k] >= 0 && nc + dc[k] < n && maze[nr + dr[k]][nc + dc[k]] == 0) {
                    nr += dr[k];
                    nc += dc[k];
                    nd++;
                    if (nr == hole[0] && nc == hole[1]) break;
                }
                if (nr == cur.r && nc == cur.c) continue;
                String np = cur.p + ch[k];
                if (nd < dist[nr][nc] || (nd == dist[nr][nc] && (path[nr][nc] == null || np.compareTo(path[nr][nc]) < 0))) {
                    dist[nr][nc] = nd;
                    path[nr][nc] = np;
                    pq.offer(new State(nd, nr, nc, np));
                }
            }
        }
        return "impossible";
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int idx = 0, t = Integer.parseInt(v[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int m = Integer.parseInt(v[idx++]), n = Integer.parseInt(v[idx++]);
            int[][] maze = new int[m][n];
            for (int i = 0; i < m; i++) for (int j = 0; j < n; j++) maze[i][j] = Integer.parseInt(v[idx++]);
            int[] ball = {Integer.parseInt(v[idx++]), Integer.parseInt(v[idx++])};
            int[] hole = {Integer.parseInt(v[idx++]), Integer.parseInt(v[idx++])};
            if (tc > 0) out.append("\n\n");
            out.append(solve(maze, ball, hole));
        }
        System.out.print(out);
    }
}
