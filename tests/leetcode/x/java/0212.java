import java.util.*;

public class Main {
    static class Node {
        Map<Character, Node> children = new HashMap<>();
        String word;
    }

    static List<String> solve(char[][] board, String[] words) {
        Node root = new Node();
        for (String word : words) {
            Node node = root;
            for (char ch : word.toCharArray()) {
                node = node.children.computeIfAbsent(ch, k -> new Node());
            }
            node.word = word;
        }
        int rows = board.length, cols = board[0].length;
        List<String> found = new ArrayList<>();
        class DFS {
            void run(int r, int c, Node node) {
                char ch = board[r][c];
                Node next = node.children.get(ch);
                if (next == null) return;
                if (next.word != null) {
                    found.add(next.word);
                    next.word = null;
                }
                board[r][c] = '#';
                if (r > 0 && board[r - 1][c] != '#') run(r - 1, c, next);
                if (r + 1 < rows && board[r + 1][c] != '#') run(r + 1, c, next);
                if (c > 0 && board[r][c - 1] != '#') run(r, c - 1, next);
                if (c + 1 < cols && board[r][c + 1] != '#') run(r, c + 1, next);
                board[r][c] = ch;
            }
        }
        DFS dfs = new DFS();
        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
                if (root.children.containsKey(board[r][c])) dfs.run(r, c, root);
        Collections.sort(found);
        return found;
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
            char[][] board = new char[rows][];
            for (int i = 0; i < rows; i++) board[i] = toks[idx++].toCharArray();
            int n = Integer.parseInt(toks[idx++]);
            String[] words = new String[n];
            for (int i = 0; i < n; i++) words[i] = toks[idx++];
            List<String> ans = solve(board, words);
            if (tc > 0) out.append("\n\n");
            out.append(ans.size());
            for (String s : ans) out.append('\n').append(s);
        }
        System.out.print(out);
    }
}
