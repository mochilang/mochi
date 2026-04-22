import java.io.*;
import java.util.*;

public class Main {
    static int minTotalDistance(int[][] grid) {
        ArrayList<Integer> rows = new ArrayList<>();
        ArrayList<Integer> cols = new ArrayList<>();
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                if (grid[i][j] == 1) rows.add(i);
            }
        }
        for (int j = 0; j < grid[0].length; j++) {
            for (int i = 0; i < grid.length; i++) {
                if (grid[i][j] == 1) cols.add(j);
            }
        }
        int mr = rows.get(rows.size() / 2);
        int mc = cols.get(cols.size() / 2);
        int ans = 0;
        for (int r : rows) ans += Math.abs(r - mr);
        for (int c : cols) ans += Math.abs(c - mc);
        return ans;
    }

    public static void main(String[] args) throws Exception {
        Scanner sc = new Scanner(System.in);
        if (!sc.hasNextInt()) return;
        int t = sc.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int r = sc.nextInt();
            int c = sc.nextInt();
            int[][] grid = new int[r][c];
            for (int i = 0; i < r; i++) {
                for (int j = 0; j < c; j++) grid[i][j] = sc.nextInt();
            }
            if (tc > 0) out.append("\n\n");
            out.append(minTotalDistance(grid));
        }
        System.out.print(out);
    }
}
