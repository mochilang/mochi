import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class Main {
    private static String solve(String[] words) {
        TreeSet<Character> chars = new TreeSet<>();
        for (String w : words) for (char c : w.toCharArray()) chars.add(c);
        Map<Character, Set<Character>> adj = new HashMap<>();
        Map<Character, Integer> indeg = new HashMap<>();
        for (char c : chars) {
            adj.put(c, new TreeSet<>());
            indeg.put(c, 0);
        }
        for (int i = 0; i + 1 < words.length; i++) {
            String a = words[i], b = words[i + 1];
            int m = Math.min(a.length(), b.length());
            if (a.substring(0, m).equals(b.substring(0, m)) && a.length() > b.length()) return "";
            for (int j = 0; j < m; j++) {
                char x = a.charAt(j), y = b.charAt(j);
                if (x != y) {
                    if (adj.get(x).add(y)) indeg.put(y, indeg.get(y) + 1);
                    break;
                }
            }
        }
        PriorityQueue<Character> pq = new PriorityQueue<>();
        for (char c : chars) if (indeg.get(c) == 0) pq.offer(c);
        StringBuilder out = new StringBuilder();
        while (!pq.isEmpty()) {
            char c = pq.poll();
            out.append(c);
            for (char nei : adj.get(c)) {
                indeg.put(nei, indeg.get(nei) - 1);
                if (indeg.get(nei) == 0) pq.offer(nei);
            }
        }
        return out.length() == chars.size() ? out.toString() : "";
    }

    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String first = reader.readLine();
        if (first == null) return;
        int t = Integer.parseInt(first.trim());
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(reader.readLine().trim());
            String[] words = new String[n];
            for (int i = 0; i < n; i++) words[i] = reader.readLine().trim();
            if (tc > 0) out.append('\n');
            out.append(solve(words));
        }
        System.out.print(out);
    }
}
