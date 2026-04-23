import java.io.*;
import java.util.*;

class Main {
    static boolean canForm(String word, HashSet<String> seen) {
        if (seen.isEmpty()) return false;
        boolean[] dp = new boolean[word.length() + 1];
        dp[0] = true;
        for (int i = 1; i <= word.length(); i++) {
            for (int j = 0; j < i; j++) {
                if (dp[j] && seen.contains(word.substring(j, i))) {
                    dp[i] = true;
                    break;
                }
            }
        }
        return dp[word.length()];
    }

    static String format(ArrayList<String> words) {
        Collections.sort(words);
        StringBuilder out = new StringBuilder("[");
        for (int i = 0; i < words.size(); i++) {
            if (i > 0) out.append(",");
            out.append("\"").append(words.get(i)).append("\"");
        }
        return out.append("]").toString();
    }

    public static void main(String[] args) throws Exception {
        String text = new String(System.in.readAllBytes()).trim();
        if (text.isEmpty()) return;
        String[] v = text.split("\\s+");
        int idx = 0;
        int t = Integer.parseInt(v[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(v[idx++]);
            ArrayList<String> words = new ArrayList<>();
            for (int i = 0; i < n; i++) words.add(v[idx++]);
            ArrayList<String> ordered = new ArrayList<>(words);
            ordered.sort((a, b) -> a.length() != b.length() ? a.length() - b.length() : a.compareTo(b));
            HashSet<String> seen = new HashSet<>();
            ArrayList<String> ans = new ArrayList<>();
            for (String word : ordered) {
                if (canForm(word, seen)) ans.add(word);
                seen.add(word);
            }
            if (tc > 0) out.append("\n\n");
            out.append(format(ans));
        }
        System.out.print(out);
    }
}
