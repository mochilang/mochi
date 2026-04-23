import java.io.*;
import java.util.*;

class Main {
    static String shrinkBoard(String s) {
        boolean changed = true;
        while (changed) {
            changed = false;
            StringBuilder out = new StringBuilder();
            for (int i = 0; i < s.length();) {
                int j = i;
                while (j < s.length() && s.charAt(j) == s.charAt(i)) j++;
                if (j - i >= 3) changed = true;
                else out.append(s, i, j);
                i = j;
            }
            s = out.toString();
        }
        return s;
    }

    static int colorId(char c) { return "RYBGW".indexOf(c); }

    static String key(String board, int[] hand) {
        return board + "|" + hand[0] + "," + hand[1] + "," + hand[2] + "," + hand[3] + "," + hand[4];
    }

    static int dfs(String board, int[] hand, HashMap<String, Integer> memo) {
        board = shrinkBoard(board);
        if (board.isEmpty()) return 0;
        String key = key(board, hand);
        if (memo.containsKey(key)) return memo.get(key);
        int inf = (int)1e9, best = inf;
        for (int i = 0; i < board.length();) {
            int j = i;
            while (j < board.length() && board.charAt(j) == board.charAt(i)) j++;
            int need = Math.max(0, 3 - (j - i));
            int id = colorId(board.charAt(i));
            if (hand[id] >= need) {
                hand[id] -= need;
                int sub = dfs(board.substring(0, i) + board.substring(j), hand, memo);
                if (sub != inf) best = Math.min(best, need + sub);
                hand[id] += need;
            }
            i = j;
        }
        memo.put(key, best);
        return best;
    }

    static int solve(String board, String handStr) {
        int[] hand = new int[5];
        for (char c : handStr.toCharArray()) hand[colorId(c)]++;
        int ans = dfs(board, hand, new HashMap<>());
        return ans >= (int)1e9 ? -1 : ans;
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int idx = 0, t = Integer.parseInt(v[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            String board = v[idx++], hand = v[idx++];
            if (tc > 0) out.append("\n\n");
            out.append(solve(board, hand));
        }
        System.out.print(out);
    }
}
