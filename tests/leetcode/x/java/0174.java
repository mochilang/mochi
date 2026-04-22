import java.util.*;

public class Main {
    static int solve(int[][] dungeon) {
        int cols = dungeon[0].length;
        long inf = (long) 4e18;
        long[] dp = new long[cols + 1];
        Arrays.fill(dp, inf);
        dp[cols - 1] = 1;
        for (int i = dungeon.length - 1; i >= 0; i--) {
            for (int j = cols - 1; j >= 0; j--) {
                long need = Math.min(dp[j], dp[j + 1]) - dungeon[i][j];
                dp[j] = need <= 1 ? 1 : need;
            }
        }
        return (int) dp[0];
    }

    public static void main(String[] args) throws Exception {
        String input = new String(System.in.readAllBytes()).trim();
        if (input.isEmpty()) return;
        String[] toks = input.split("\\s+");
        int idx = 0, t = Integer.parseInt(toks[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int rows = Integer.parseInt(toks[idx++]);
            int cols = Integer.parseInt(toks[idx++]);
            int[][] dungeon = new int[rows][cols];
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    dungeon[i][j] = Integer.parseInt(toks[idx++]);
                }
            }
            if (tc > 0) out.append('\n');
            out.append(solve(dungeon));
        }
        System.out.print(out);
    }
}
