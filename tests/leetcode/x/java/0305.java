import java.io.*;
import java.util.*;

public class Main {
    static List<Integer> solve(int m, int n, int[][] positions) {
        HashMap<Integer, Integer> parent = new HashMap<>();
        HashMap<Integer, Integer> rank = new HashMap<>();
        List<Integer> ans = new ArrayList<>();
        int count = 0;

        for (int[] p : positions) {
            int r = p[0], c = p[1];
            int idx = r * n + c;
            if (parent.containsKey(idx)) {
                ans.add(count);
                continue;
            }
            parent.put(idx, idx);
            rank.put(idx, 0);
            count++;
            int[][] dirs = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
            for (int[] d : dirs) {
                int nr = r + d[0], nc = c + d[1];
                if (nr >= 0 && nr < m && nc >= 0 && nc < n) {
                    int nei = nr * n + nc;
                    if (parent.containsKey(nei) && union(idx, nei, parent, rank)) count--;
                }
            }
            ans.add(count);
        }
        return ans;
    }

    static int find(int x, HashMap<Integer, Integer> parent) {
        while (parent.get(x) != x) {
            parent.put(x, parent.get(parent.get(x)));
            x = parent.get(x);
        }
        return x;
    }

    static boolean union(int a, int b, HashMap<Integer, Integer> parent, HashMap<Integer, Integer> rank) {
        int ra = find(a, parent), rb = find(b, parent);
        if (ra == rb) return false;
        if (rank.get(ra) < rank.get(rb)) {
            int t = ra; ra = rb; rb = t;
        }
        parent.put(rb, ra);
        if (Objects.equals(rank.get(ra), rank.get(rb))) rank.put(ra, rank.get(ra) + 1);
        return true;
    }

    static String fmtList(List<Integer> a) {
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < a.size(); i++) {
            if (i > 0) sb.append(',');
            sb.append(a.get(i));
        }
        sb.append(']');
        return sb.toString();
    }

    public static void main(String[] args) throws Exception {
        Scanner sc = new Scanner(System.in);
        if (!sc.hasNextInt()) return;
        int t = sc.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int m = sc.nextInt(), n = sc.nextInt(), k = sc.nextInt();
            int[][] positions = new int[k][2];
            for (int i = 0; i < k; i++) {
                positions[i][0] = sc.nextInt();
                positions[i][1] = sc.nextInt();
            }
            if (tc > 0) out.append("\n\n");
            out.append(fmtList(solve(m, n, positions)));
        }
        System.out.print(out);
    }
}
